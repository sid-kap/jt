module Jt.Server (
    Server(..),
    HistoryUrl(..),
    AppUrl(..),
    combineEither,
    appJobs,
    appJobsWithOpts,
    historyJobs,
    historyJobsWithOpts,
    ) where

import Jt.Job (Job)
import qualified Jt.History.Listing as History
import qualified Jt.App.Listing as App
import qualified Jt.QueryParameters as QP
import Data.List (nub)

newtype HistoryUrl = HistoryUrl String deriving (Show, Eq, Ord)

newtype AppUrl = AppUrl String deriving (Show, Eq, Ord)

data Server = Server
  { serverName :: String
  , appUrl :: AppUrl
  , historyUrl :: HistoryUrl
  } deriving (Show, Eq, Ord)

combineEither :: Either String [a] -> Either String [a] -> Either String [a]
combineEither (Left e1) (Left e2) = Left $ e1 ++ "\n" ++ e2
combineEither (Left e1) (Right _) = Left e1
combineEither (Right _) (Left e2) = Left e2
combineEither (Right r1) (Right r2) = Right $ r1 ++ r2

appJobs :: AppUrl -> IO (Either String [Job])
appJobs (AppUrl url) = App.fetchJobs QP.defaultsQP url

appJobsWithOpts :: QP.QueryParameters -> AppUrl -> IO (Either String [Job])
appJobsWithOpts opts (AppUrl url) = App.fetchJobs opts url

historyJobs :: HistoryUrl -> IO (Either String [Job])
historyJobs (HistoryUrl url) = History.fetchJobs QP.defaultsQP url

historyJobsWithOpts :: QP.QueryParameters -> HistoryUrl -> IO (Either String [Job])
historyJobsWithOpts opts (HistoryUrl url) = History.fetchJobs opts url

serverJobs :: Server -> IO (Either String [Job])
serverJobs s = do
  apps <- appJobs $ appUrl s
  historyJobs <- historyJobs $ historyUrl s
  let allJobs = combineEither apps historyJobs
  return $ fmap nub allJobs

serverJobsWithOpts :: QP.QueryParameters -> Server -> IO (Either String [Job])
serverJobsWithOpts opts s = do
  apps <- appJobsWithOpts opts $ appUrl s
  historyJobs <- historyJobsWithOpts opts $ historyUrl s
  let allJobs = combineEither apps historyJobs
  return $ fmap nub allJobs
