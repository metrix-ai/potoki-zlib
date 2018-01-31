module Potoki.Zlib.Transform
(
  decompress
)
where

import Potoki.Zlib.Prelude
import Potoki.Core.Transform
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Zlib.Fetch as C
import qualified Codec.Compression.Zlib as B


decompress :: Transform ByteString (Either B.DecompressionError ByteString)
decompress =
  Transform $ \ oldFetch -> do
    unfetchedChunksRef <- newIORef []
    resultRef <- newIORef B.decompressIncremental
    return (C.zlibResult unfetchedChunksRef resultRef oldFetch)
