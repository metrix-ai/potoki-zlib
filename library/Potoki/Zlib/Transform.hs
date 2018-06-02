module Potoki.Zlib.Transform
(
  decompress
)
where

import Potoki.Zlib.Prelude
import qualified Potoki.Zlib.Fetch as Z
import Potoki.Core.Transform (Transform(..))
import qualified Codec.Compression.Zlib.Internal as I
import qualified Acquire.Acquire as A


decompress :: Transform ByteString (Either I.DecompressError ByteString)
decompress =
  Transform $ \oldFetchIO -> A.Acquire $ do
    initChunksRef <- newIORef []
    decompRef <- newIORef (I.decompressIO I.gzipFormat I.defaultDecompressParams)
    let
      fetch =
        Z.runGzip initChunksRef decompRef oldFetchIO
      kill =
        return ()
    return (fetch, kill)

