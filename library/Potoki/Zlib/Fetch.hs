module Potoki.Zlib.Fetch (
  runGzip,
  withCounter,
) where

import Potoki.Zlib.Prelude
import Potoki.Core.Fetch
import Potoki.Core.Consume
import qualified Codec.Compression.Zlib.Internal as A


withConsumeHook :: Consume a b -> (a -> IO ()) -> Consume a b
withConsumeHook (Consume consume) hook =
  Consume $ \ (Fetch fetchIO) -> consume $ Fetch $ do
    fetch <- fetchIO
    case fetch of
      Nothing -> return Nothing
      Just val -> Just val <$ hook val

withCounter :: Consume a b -> (Int -> IO ()) -> Consume a b
withCounter consume int2IO =
  do
    counterVar <- liftIO $ newIORef 0
    withConsumeHook consume $ \_ -> do
      modifyIORef' counterVar succ
      readIORef counterVar >>= int2IO

runGzip :: IORef [ByteString]
        -> IORef (A.DecompressStream IO)
        -> Fetch ByteString
        -> Fetch (Either A.DecompressError ByteString)
runGzip unfetchedChunksRef resultRef (Fetch oldFetchIO) =
  Fetch $ do
    let
      interpretResult resultIO = do
        result <- resultIO
        case result of
          A.DecompressInputRequired nextResult -> do
            newResult <- do
              oldFetch <- oldFetchIO
              return $ case oldFetch of
                Nothing  -> nextResult mempty
                Just val -> nextResult val
            interpretResult newResult
          A.DecompressOutputAvailable decompressOutput decompressNext -> do
            --                     :: !S.ByteString -> m (DecompressStream m)
            nextResult <- decompressNext
            writeIORef resultRef nextResult
            -- writeIORef unfetchedChunksRef nextResult -- TODO: not needed??
            return (Just (Right decompressOutput))
          A.DecompressStreamEnd _ ->
            return Nothing
          A.DecompressStreamError err ->
            return (Just (Left err))
    unfetchedChunks <- readIORef unfetchedChunksRef
    case unfetchedChunks of
      headChunk : unfetchedChunksTail -> do
        writeIORef unfetchedChunksRef unfetchedChunksTail
        return (Just (Right headChunk))
      _ -> interpretResult $ readIORef resultRef
