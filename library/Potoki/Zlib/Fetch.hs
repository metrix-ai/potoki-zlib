module Potoki.Zlib.Fetch
where

import Potoki.Zlib.Prelude
import Potoki.Core.Fetch
import qualified Codec.Compression.Zlib as B
import qualified Data.ByteString.Lazy as C


zlibResult :: IORef [ByteString] -> IORef B.ZlibDecoder -> Fetch ByteString -> Fetch (Either B.DecompressionError ByteString)
zlibResult unfetchedChunksRef resultRef (Fetch oldFetchIO) =
  Fetch $ \ nil just ->
  let
    interpretResult result =
      case result of
        B.NeedMore nextResult ->
          do
            newResult <- oldFetchIO (nextResult mempty) nextResult
            interpretResult newResult
        B.Chunk decodedLazyChunk nextResult ->
          case C.toChunks decodedLazyChunk of
            (headChunk : tailChunks) -> do
              writeIORef resultRef nextResult
              writeIORef unfetchedChunksRef tailChunks
              return (just (Right headChunk))
            _ -> interpretResult nextResult
        B.Done ->
          return nil
        B.DecompError error ->
          return (just (Left error))
    in do
      unfetchedChunks <- readIORef unfetchedChunksRef
      case unfetchedChunks of
        headChunk : unfetchedChunksTail -> do
          writeIORef unfetchedChunksRef unfetchedChunksTail
          return (just (Right headChunk))
        _ -> do
          result <- readIORef resultRef
          interpretResult result
