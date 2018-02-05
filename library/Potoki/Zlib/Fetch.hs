{-# LANGUAGE PartialTypeSignatures #-}

module Potoki.Zlib.Fetch (
  runGzip
) where

import Potoki.Zlib.Prelude

import Potoki.Core.Fetch

import qualified Codec.Compression.Zlib.Internal as Z

runGzip :: IORef [ByteString]
        -> IORef (Z.DecompressStream IO)
        -> Fetch ByteString
        -> Fetch (Either Z.DecompressError ByteString)
runGzip unfetchedChunksRef resultRef (Fetch oldFetchIO) =
  Fetch $ \nil just ->
  let
    interpretResult result =
      case result of

        Z.DecompressInputRequired nextResult{-decompressSupplyInput-} -> do
          newResult <- join $ oldFetchIO (nextResult mempty) nextResult
          interpretResult newResult

        -- B.Chunk decodedLazyChunk nextResult ->
        --   case C.toChunks decodedLazyChunk of
        --     (headChunk : tailChunks) -> do
        --       writeIORef resultRef nextResult
        --       writeIORef unfetchedChunksRef tailChunks
        --       return (just (Right headChunk))
        --     _ -> interpretResult nextResult

        Z.DecompressOutputAvailable decompressOutput decompressNext -> do
          --                     :: !S.ByteString -> m (DecompressStream m)
          nextResult <- decompressNext
          writeIORef resultRef nextResult
          -- writeIORef unfetchedChunksRef nextResult -- TODO: not needed
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

