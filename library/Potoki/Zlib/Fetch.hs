module Potoki.Zlib.Fetch (
  runGzip
) where

import           Potoki.Core.Fetch

import qualified Codec.Compression.Zlib.Internal as Z

import           Control.Monad (join)
import           Data.ByteString
import           Data.Either
import           Data.IORef
import           Prelude

runGzip :: IORef [ByteString]
        -> IORef (Z.DecompressStream IO)
        -> Fetch ByteString
        -> Fetch (Either Z.DecompressError ByteString)
runGzip unfetchedChunksRef resultRef (Fetch oldFetchIO) =
  Fetch $ \nil just ->
  let
    interpretResult result =
      case result of

        Z.DecompressInputRequired nextResult -> do
          newResult <- join $ oldFetchIO (nextResult mempty) nextResult
          interpretResult newResult

        Z.DecompressOutputAvailable decompressOutput decompressNext -> do
          --                     :: !S.ByteString -> m (DecompressStream m)
          nextResult <- decompressNext
          writeIORef resultRef nextResult
          -- writeIORef unfetchedChunksRef nextResult -- TODO: not needed??
          return (just (Right decompressOutput))

        Z.DecompressStreamEnd _ ->
          return nil

        Z.DecompressStreamError err ->
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

