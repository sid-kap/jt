{-# LANGUAGE OverloadedStrings #-}

module Jt.Net (
    queryUrl,
    queryUrlWith,
    fetchJsonUrl
    ) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson (FromJSON, eitherDecode)
import Network.HTTP.Client(HttpException(..))
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Jt.QueryParameters as QP
import qualified Jt.Utils as Utils

queryUrlWith :: QP.QueryParameters -> String -> IO (Either String BL.ByteString)
queryUrlWith params' url = do
  run <- try (getWith options' url)
  return $ case run of
    Right a -> Right $ a ^. responseBody
    Left (StatusCodeException _ _ _) -> Left "TooManyRedirects"
    Left e -> Left $ show (e :: HttpException)
  where
    options' = defaults
      & params .~ map (\(QP.QueryParameter k v) -> (k,v)) params'
      & redirects .~ 0

queryUrl :: String -> IO(Either String BL.ByteString)
queryUrl = queryUrlWith QP.defaultsQP

eitherDecodePretty :: FromJSON a => BL.ByteString -> Either String a
eitherDecodePretty bs = case eitherDecode bs of
  Right a -> Right a
  Left l  -> Left ("Unable to decode response:\n" ++ BL.unpack bs ++ "\n\nError: " ++ l)

redirectToNothing :: Either String a -> Either String (Maybe a)
redirectToNothing (Left "TooManyRedirects") = Right Nothing
redirectToNothing (Right a) = Right (Just a)
redirectToNothing (Left a) = Left a

fetchJsonUrl :: FromJSON a => QP.QueryParameters -> String -> IO (Either String (Maybe a))
fetchJsonUrl params' finalUrl = do
  jInfoEither <- (>>= eitherDecodePretty) <$> queryUrlWith params' finalUrl
  let
    resApps = jInfoEither
    withNoJob = redirectToNothing resApps
    resApps' = Utils.addInfo ("Url Queried: " ++ finalUrl ++ "\n") $ withNoJob
  return resApps'
