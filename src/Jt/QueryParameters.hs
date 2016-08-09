{-# LANGUAGE OverloadedStrings #-}
module Jt.QueryParameters (
    QueryParameters(..),
    QueryParameter(..),
    defaultsQP,
    ) where

import Data.Text (Text)

data QueryParameter = QueryParameter
  { key   :: Text
  , value :: Text
  } deriving (Show, Eq)

type QueryParameters = [QueryParameter]

defaultsQP :: QueryParameters
defaultsQP = [QueryParameter "limit" "10"]
