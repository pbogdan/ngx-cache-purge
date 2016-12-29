module JobQueue.Types
  (PurgeJob(..))

where

import Data.ByteString (ByteString)

data PurgeJob = PurgeJob
  { pjHost :: ByteString
  , pjPath :: ByteString
  } deriving (Show, Eq)
