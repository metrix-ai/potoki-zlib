module Potoki.Zlib.Fetch (
  runGzip,
  withCounter
) where

import           Potoki.Core.Consume (Consume(..))
import           Potoki.Core.Fetch

import qualified Codec.Compression.Zlib.Internal as Z

import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString
import           Data.Either
import           Data.IORef
import           Prelude

withConsumeHook :: Consume a b -> (a -> IO ()) -> Consume a b
withConsumeHook (Consume consume) hook = Consume $ \(Fetch fetch) -> consume $
  Fetch $ \nil just -> join $ fetch (return nil) (\x -> just x <$ hook x)

withCounter :: Consume a b -> (Int -> IO ()) -> Consume a b
withCounter consume k = do
  counterVar <- liftIO $ newIORef 0
  withConsumeHook consume $ \_ -> do
    modifyIORef' counterVar succ
    readIORef counterVar >>= k

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

