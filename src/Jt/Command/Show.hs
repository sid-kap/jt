module Jt.Command.Show (
  showCommand
) where

import Jt.Server
import Jt
import Options.Applicative
import Text.Printf
import Control.Monad
import qualified Data.Map.Strict as Map

data ShowArgs = ShowArgs { showHistory :: Bool
                         , showRM :: Bool
                         }

showCommand :: Command
showCommand = Command { commandName = "show"
                      , commandDesc = "Show clusters"
                      , commandParser = showParser
                      , commandAction = showAction }

showParser :: Parser ShowArgs
showParser =
  let
    history = switch (long "history" <> help "History: show the history url")
    rm = switch (long "resource-manager" <> help "resource-manager: show the rm url")
  in ShowArgs <$> history <*> rm

printServer :: Server -> Bool -> Bool -> IO ()
printServer server historyInclude rmInclude = do
    printf "\t%s\t" (serverName server)
    when historyInclude $ printf "%s\t" (historyUrlStr server)
    when rmInclude      $ printf "%s\t" (rmUrlStr server)
  where
    historyUrlStr (Server _ _ (HistoryUrl url)) = url
    rmUrlStr      (Server _ (AppUrl url) _) = url


printServerLn :: Bool -> Bool -> Server -> IO ()
printServerLn historyInclude rmInclude server = do
    printServer server historyInclude rmInclude
    printf "\n"

showAction :: Config -> ShowArgs -> IO ()
showAction cfg sargs = do
  printf "Default:\n"
  printServerLn' defaultSrv'
  printf "\nOthers:\n"
  mapM_ printServerLn' nonDefaultServers
  printf "\n"
  where
    particularSet  = showRM sargs || showHistory sargs
    historyInclude = not particularSet || showHistory sargs
    rmInclude      = not particularSet || showRM sargs
    printServerLn' = printServerLn historyInclude rmInclude
    defaultSrv' = defaultServer cfg
    servers'    = configServers cfg
    nonDefaultServers = Map.filter (/= defaultSrv') servers'
