module Potoki.Zlib.Transform
(
  decompress
)
where

-- TODO: import ByteString
import Potoki.Zlib.Prelude

import Potoki.Zlib.Fetch (runGzip)

import Potoki.Core.Transform

import qualified Codec.Compression.Zlib.Internal as Z

decompress :: Transform ByteString (Either Z.DecompressError ByteString)
decompress =
  Transform $ \oldFetch -> do
    unfetchedChunksRef <- newIORef []
    initChunksRef <- newIORef []
    decompRef <- newIORef (Z.decompressIO
                           Z.gzipFormat
                           Z.defaultDecompressParams)
    return (runGzip initChunksRef decompRef oldFetch)

