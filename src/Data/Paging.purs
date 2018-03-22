module Ocelot.Data.Paging where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))

-----
-- Type definition for the paging component of paged responses

newtype Paging = Paging
  { skip :: Int
  , count :: Int
  , last :: Int
  , limit :: Int
  }

instance decodeJsonPaging :: DecodeJson Paging where
  decodeJson json = do
    obj <- decodeJson json
    skip <- obj .? "skip"
    count <- obj .? "count"
    last <- obj .? "last"
    limit <- obj .? "limit"
    pure $ Paging { skip, count, last, limit }

instance showPaging :: Show Paging where
  show (Paging { skip, limit }) = "&skip=" <> show skip <> "&limit=" <> show limit


