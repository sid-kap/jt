{-# LANGUAGE RecordWildCards #-}
module Jt.Command.Jobs (
  jobsAction,
  jobsCommand
) where

import Jt
import Jt.Utils (failOnLeft')
import qualified Jt.Job as Job
import Jt.Server
import Data.Maybe (fromMaybe)
import Options.Applicative
import qualified Jt.QueryParameters as QP
import Jt.Command.Utils
import Data.List (intercalate)
import Options.Applicative.Types
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data ValidStates = RUNNING | SUCCESSFUL | FAILED | KILLED

parseValidStates :: ReadM ValidStates
parseValidStates = do
      input <- readerAsk
      extractFn input
  where
    extractFn "running"    = return RUNNING
    extractFn "successful" = return SUCCESSFUL
    extractFn "failed"     = return FAILED
    extractFn "killed"     = return KILLED
    extractFn input        = readerError ("Invalid state: " ++ input ++ "\nOnly:  running, successful, failed or killed are supported")


data JobArgs = JobArgs { jobUser :: Maybe String
                       , jobCluster :: Maybe String
                       , jobLimit :: Maybe Int
                       , showHistory :: Bool
                       , showRM :: Bool
                       , jobState :: [ValidStates]
                       , jobTabs :: Bool
                       }

jobsCommand :: Command
jobsCommand = Command { commandName = "jobs"
                      , commandDesc = "List Jobs"
                      , commandParser = jobsParser
                      , commandAction = jobsAction }

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

-- NEW, NEW_SAVING, SUBMITTED, ACCEPTED, RUNNING, FINISHED, FAILED, KILLED
statesToAMStates :: [ValidStates] -> Maybe String
statesToAMStates [] = Nothing
statesToAMStates arr = Just (intercalate "," $ arr >>= convertToString)
    where
        convertToString RUNNING    = ["RUNNING"]
        convertToString SUCCESSFUL = ["FINISHED"]
        convertToString FAILED     = ["FAILED"]
        convertToString KILLED     = ["KILLED"]


-- NEW, INITED, RUNNING, SUCCEEDED, FAILED, KILL_WAIT, KILLED, ERROR
-- Error can be in a response it seems but not request. Oh hadoop.
statesToHistoryStates :: [ValidStates] -> Maybe String
statesToHistoryStates [] = Nothing
statesToHistoryStates arr = Just (intercalate "," $ arr >>= convertToString)
    where
        convertToString RUNNING    = ["RUNNING"]
        convertToString SUCCESSFUL = ["SUCCEEDED"]
        convertToString FAILED     = ["FAILED"]
        convertToString KILLED     = ["KILLED"]


printResults :: Config -> JobArgs -> IO ()
printResults conf sargs = do
  let
    particularSet  = showRM sargs || showHistory sargs
    historyInclude = not particularSet || showHistory sargs
    rmInclude      = not particularSet || showRM sargs

    userOption         = fromMaybe QP.EmptyParameter $ fmap (QP.toQp "user") (jobUser sargs)
    limitOption        = fromMaybe QP.EmptyParameter $ fmap (QP.toQp "limit" . show) (jobLimit sargs)
    appStateOption     = fromMaybe QP.EmptyParameter $ fmap (QP.toQp "states") (statesToAMStates $ jobState sargs)
    historyStateOption = fromMaybe QP.EmptyParameter $ fmap (QP.toQp "state") (statesToHistoryStates $ jobState sargs)

    maxLimit = fromMaybe 500 $ jobLimit sargs
    server = extractServer conf (jobCluster sargs)

    appQueryParameters = QP.QueryParameters [userOption, limitOption, appStateOption]
    historyQueryParameters = QP.QueryParameters [userOption, limitOption, historyStateOption]

  historyJobs <- if historyInclude
                   then Job.jobsWithOpts historyQueryParameters $ historyUrl server
                   else return $ Right []

  rmJobs <- if rmInclude
              then Job.jobsWithOpts appQueryParameters $ appUrl server
              else return $ Right []

  let
    jobLimited = map toLineSummary . take maxLimit <$> combineEither rmJobs historyJobs

  summarizedJobs <- failOnLeft' jobLimited

  let
    column = if jobTabs sargs then tabColumnarize else evenColumnarize
    lineSummaries = column (headLine : summarizedJobs)

  mapM_ putStrLn lineSummaries

jobsAction :: Config -> JobArgs -> IO ()
jobsAction conf sargs = do
  printResults conf sargs
