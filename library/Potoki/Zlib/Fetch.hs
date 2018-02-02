{-# LANGUAGE PartialTypeSignatures #-}

module Potoki.Zlib.Fetch (
  gzip,
  gzipResult
) where

import Potoki.Zlib.Prelude

import Potoki.Core.Fetch

import qualified Codec.Compression.Zlib.Internal as Z

-- import qualified Data.ByteString.Lazy as BL

gzip :: IORef (Z.DecompressStream IO)
     -> Fetch ByteString
     -> Fetch (Either Z.DecompressError ByteString)
gzip decompRef (Fetch oldFetchIO) =
  Fetch $ \ nil just -> do
    stream <- readIORef decompRef
    case stream of
      Z.DecompressInputRequired nxt -> do
        error "DecompressInputRequired"
    -- newResult <- oldFetchIO (nextResult mempty) nextResult
    -- error newResult
    return (just (Right undefined)) --oldFetchIO {-headChunk-}))


gzipResult :: IORef [ByteString]
           -> IORef (Z.DecompressStream IO)
           -> Fetch ByteString
           -> Fetch (Either Z.DecompressError ByteString)
gzipResult unfetchedChunksRef resultRef (Fetch oldFetchIO) =
{--
 undefined
--}
--{--
  Fetch $ \ nil just ->
  let
    interpretResult result =
      case result of

        Z.DecompressInputRequired nextResult{-decompressSupplyInput-} -> do
          error "DecompressInputRequired"
          -- newResult <- oldFetchIO (nextResult mempty) nextResult
          -- interpretResult newResult -- _ -- TODO: type hole

        Z.DecompressOutputAvailable decompressOutput decompressNext -> do
          nextResult <- decompressNext
          writeIORef resultRef nextResult
          -- writeIORef unfetchedChunksRef nextResult -- TODO: not needed
          return (just (Right decompressOutput))

   -- | DecompressOutputAvailable {
   --       decompressOutput :: !S.ByteString,
   --       decompressNext   :: m (DecompressStream m)
   --     }

-- fold (DecompressInputRequired next) [] =
--   next S.empty >>= \strm -> fold strm []
-- fold (DecompressInputRequired next) (inchunk:inchunks) =
--   next inchunk >>= \s -> fold s inchunks

        Z.DecompressStreamEnd _ ->
          traceShow "END"
          return nil

        Z.DecompressStreamError err ->
          traceShow "ERROR"
          return (just (Left err))

    in do
      unfetchedChunks <- readIORef unfetchedChunksRef
      case unfetchedChunks of
        headChunk : unfetchedChunksTail -> do
          writeIORef unfetchedChunksRef unfetchedChunksTail
          return (just (Right headChunk))
        _ -> do
          result <- readIORef resultRef
          interpretResult result
--}

