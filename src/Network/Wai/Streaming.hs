{-# LANGUAGE DeriveFunctor #-}

module Network.Wai.Streaming ( Flush(..)
                             -- * ByteStrings
                             , streamingResponse
                             , streamingBody

                             -- * Flushed Builders
                             -- $flush
                             , streamingResponseF
                             , streamingBodyF
                             ) where

import Streaming
import Network.Wai
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, Builder)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Streaming.Prelude as S

data Flush a = Chunk !a
             | Flush
             deriving (Show, Functor)


-- | Stream strict 'ByteString's into a 'Response'
streamingResponse :: Stream (Of ByteString) IO r
                  -> Status
                  -> ResponseHeaders
                  -> Response
streamingResponse src status headers =
  responseStream status headers (streamingBody src)


-- | Stream strict 'ByteString's into a 'StreamingBody'
streamingBody :: Stream (Of ByteString) IO r -> StreamingBody
streamingBody src write flush = S.foldM_ writer flush return src
  where
    writer _ a = do
      write (byteString a)
      flush

-- $flush
--
-- Using Flush allows you to explicitly control flushing behavior

-- | Stream 'Builder's into a 'Response'
streamingResponseF :: Stream (Of (Flush Builder)) IO r
                   -> Status
                   -> ResponseHeaders
                   -> Response
streamingResponseF src status headers =
  responseStream status headers (streamingBodyF src)


-- | Stream 'Builder's into a 'StreamingBody'
streamingBodyF :: Stream (Of (Flush Builder)) IO r -> StreamingBody
streamingBodyF src write flush = S.foldM_ writer flush return src
  where
    writer _ (Chunk a) = write a
    writer _ Flush     = flush
