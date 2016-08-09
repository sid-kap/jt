{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Jt.Command.Jobs (
  jobsCommand
) where

import Jt
import Jt.Utils (failOnLeft')
import qualified Jt.Job as Job
import Jt.Server
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (pack)
import Options.Applicative
import qualified Jt.QueryParameters as QP
import Jt.Command.Utils
import Data.List (intercalate)
import Options.Applicative.Types
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data ValidState = Running | Successful | Failed | Killed

instance Show ValidState where
  show Running = "RUNNING"
  show Successful = "SUCCESSFUL"
  show Failed = "FAILED"
  show Killed = "KILLED"

parseValidStates :: ReadM ValidState
parseValidStates = do
  input <- readerAsk
  extractFn input
  where
    extractFn "running"    = return Running
    extractFn "successful" = return Successful
    extractFn "failed"     = return Failed
    extractFn "killed"     = return Killed
    extractFn input        = readerError ("Invalid state: " ++ input ++ "\nOnly:  running, successful, failed or killed are supported")


data JobArgs = JobArgs
  { jobUser :: Maybe String
  , jobCluster :: Maybe String
  , jobLimit :: Maybe Int
  , showHistory :: Bool
  , showRM :: Bool
  , jobState :: [ValidState]
  , jobTabs :: Bool
  }

jobsCommand :: Command
jobsCommand = Command { commandName = "jobs"
                      , commandDesc = "List Jobs"
                      , commandParser = jobsParser
                      , commandAction = printResults }

jobsParser :: Parser JobArgs
jobsParser = let
  clusterP = optional (strOption (long "cluster"
                                  <> short 'c'
                                  <> metavar "CLUSTER"
                                  <> help "cluster to operate from"))
  userP = optional (strOption (long "user"
                               <> short 'u'
                               <> metavar "USER"
                               <> help "user to list jobs from"))
  limitP = optional (option auto (long "limit"
                                  <> short 'l'
                                  <> metavar "LIMIT"
                                  <> help "limit of jobs to return"))
  stateP = many (option parseValidStates (long "state"
                                          <> short 's'
                                          <> metavar "STATE"
                                          <> help "filter for jobs in states"))
  history = switch (long "history"
                    <> short 'o'
                    <> help "History: show the history url")
  rm = switch (long "resource-manager"
               <> short 'a'
               <> help "resource-manager: show the rm url")
  tabs = switch (long "tabs"
                 <> short 't'
                 <> help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in JobArgs <$> userP <*> clusterP <*> limitP
             <*> history <*> rm <*> stateP <*> tabs


toLineSummary :: Job.Job -> [String]
toLineSummary Job.Job{..} =
  let
    startedTime' = show . posixSecondsToUTCTime . fromIntegral . (`div` 1000) $ startedTime
  in [name, user, state, jobId, startedTime']

headLine :: [String]
headLine = ["Name", "User", "State", "JobId", "StartedTime"]

showStatesList :: [ValidState] -> Maybe String
showStatesList [] = Nothing
showStatesList xs = Just (intercalate "," (map show xs))

printResults :: Config -> JobArgs -> IO ()
printResults conf sargs = do
  let
    particularSet  = showRM sargs || showHistory sargs
    historyInclude = not particularSet || showHistory sargs
    rmInclude      = not particularSet || showRM sargs

    userOption         = QP.QueryParameter "user"  . pack  <$> jobUser sargs
    limitOption        = QP.QueryParameter "limit" . pack . show <$> jobLimit sargs
    appStateOption     = QP.QueryParameter "states" . pack <$> showStatesList (jobState sargs)
    historyStateOption = QP.QueryParameter "state"  . pack <$> showStatesList (jobState sargs)

    maxLimit = fromMaybe 500 $ jobLimit sargs
    server = extractServer conf (jobCluster sargs)

    appQueryParameters     = catMaybes [userOption, limitOption, appStateOption]
    historyQueryParameters = catMaybes [userOption, limitOption, historyStateOption]

  historyJobs <- if historyInclude
                   then historyJobsWithOpts historyQueryParameters (historyUrl server)
                   else return $ Right []

  rmJobs <- if rmInclude
              then appJobsWithOpts appQueryParameters (appUrl server)
              else return $ Right []

  let
    jobLimited = map toLineSummary . take maxLimit <$> combineEither rmJobs historyJobs

  summarizedJobs <- failOnLeft' jobLimited

  let
    column = if jobTabs sargs then tabColumnarize else evenColumnarize
    lineSummaries = column (headLine : summarizedJobs)

  mapM_ putStrLn lineSummaries
