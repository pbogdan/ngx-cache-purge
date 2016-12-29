module JobQueue.Types
  (PurgeJob(..))

where

import Protolude.Lifted

data PurgeJob = PurgeJob
  { pjHost :: ByteString
  , pjPath :: ByteString
  } deriving (Show, Eq)
