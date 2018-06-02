module Potoki.Zlib.Fetch (
  runGzip,
  withCounter
) where

import Potoki.Zlib.Prelude
import Potoki.Core.Consume (Consume(..))
import Potoki.Core.Fetch
import qualified Codec.Compression.Zlib.Internal as C


withConsumeHook :: Consume a b -> (a -> IO ()) -> Consume a b
withConsumeHook = undefined
{-
withConsumeHook (Consume consume) hook = Consume $ \(Fetch fetch) -> consume $
  Fetch $ \nil just -> join $ fetch (return nil) (\x -> just x <$ hook x)
-}

withCounter :: Consume a b -> (Int -> IO ()) -> Consume a b
withCounter = undefined
{-
withCounter consume k = do
  counterVar <- liftIO $ newIORef 0
  withConsumeHook consume $ \_ -> do
    modifyIORef' counterVar succ
    readIORef counterVar >>= k
-}
runGzip :: IORef [ByteString]
        -> IORef (C.DecompressStream IO)
        -> Fetch ByteString
        -> Fetch (Either C.DecompressError ByteString)
runGzip = undefined
{-
runGzip unfetchedChunksRef resultRef (Fetch oldFetchIO) =
  Fetch $ do
    let
      interpretResult result =
        case result of

          C.DecompressInputRequired nextResult -> do
            newResult <- join $ oldFetchIO (nextResult mempty) nextResult
            interpretResult newResult

          C.DecompressOutputAvailable decompressOutput decompressNext -> do
            nextResult <- decompressNext
            writeIORef resultRef nextResult
            -- writeIORef unfetchedChunksRef nextResult -- TODO: not needed??
            return (Just (Right decompressOutput))

          C.DecompressStreamEnd _ ->
            return Nothing
  
          C.DecompressStreamError err ->
            return (Just (Left err))

    unfetchedChunks <- readIORef unfetchedChunksRef
    case unfetchedChunks of
      headChunk : unfetchedChunksTail -> do
        writeIORef unfetchedChunksRef unfetchedChunksTail
        return (Just (Right headChunk))
      _ -> do
        result <- readIORef resultRef
        interpretResult result
-}
