module Jt.Utils (
    toApplicationId
  , toJobId
  , addInfo
  , failOnLeft
  , failOnLeft'
    ) where

import qualified Data.String.Utils as StringUtils
import Data.Typeable
import Control.Exception

toApplicationId :: String -> String
toApplicationId jobId' = StringUtils.replace "job_" "application_" jobId'

toJobId :: String -> String
toJobId jobId' = StringUtils.replace "application_" "job_" jobId'


addInfo :: String -> Either String a -> Either String a
addInfo extra (Left l) = Left (extra ++ l)
addInfo extra o = o

data FailOnLeftException = FailOnLeftException String deriving (Show, Typeable)
instance Exception FailOnLeftException

failOnLeft :: Either String b -> b
failOnLeft (Right e) = e
failOnLeft (Left str')  = throw $ FailOnLeftException str'

failOnLeft' :: Either String b -> IO b
failOnLeft' (Right e) = return e
failOnLeft' (Left str')  = throwIO $ FailOnLeftException str'
