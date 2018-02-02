module Potoki.Zlib.Transform
(
  decompress
)
where

import Potoki.Zlib.Prelude
import Potoki.Zlib.Fetch (gzipResult, gzip)

import Potoki.Core.Transform
-- import qualified Potoki.Core.Fetch as PF

import qualified Codec.Compression.Zlib.Internal as Z

instance Show (Transform ByteString (Either Z.DecompressError ByteString)) where
  show _ = "Transform ByteString (Either ... ...)"

decompress :: Transform ByteString (Either Z.DecompressError ByteString)
decompress =
  {-
  Transform $ \ oldFetch -> do
    unfetchedChunksRef <- newIORef []
    -- resultRef <- newIORef (Z.DecompressStreamEnd "")      -- TODO: END
    -- resultRef <- newIORef $ Z.DecompressInputRequired (_) -- TODO: no input
    -- resultRef <- newIORef $ Z.DecompressInputRequired (_) -- TODO: not needed?
    resultRef <- newIORef decompressIncremental {- ZlibDecoder -}
    return (zlibResult unfetchedChunksRef resultRef oldFetch)
  -}

  Transform $ \oldFetch -> do
    unfetchedChunksRef <- newIORef []
    -- resultRef <- newIORef (Z.DecompressStreamEnd "")      -- TODO: END
    -- resultRef <- newIORef $ Z.DecompressInputRequired (_) -- TODO: no input
    -- resultRef <- newIORef $ Z.DecompressInputRequired (_) -- TODO: not needed?
    -- return (gzipResult unfetchedChunksRef resultRef oldFetch)
    initChunksRef <- newIORef []
    decompRef <- newIORef (Z.decompressIO
                           Z.gzipFormat
                           Z.defaultDecompressParams)
    -- return (gzip decompRef oldFetch)
    return (gzip initChunksRef decompRef oldFetch)

