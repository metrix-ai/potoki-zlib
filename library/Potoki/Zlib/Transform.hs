module Potoki.Zlib.Transform
(
  decompress
)
where

import Potoki.Zlib.Fetch               (runGzip)

import Potoki.Core.Transform           (Transform(..))

import Codec.Compression.Zlib.Internal (DecompressError(..),
                                        decompressIO,
                                        gzipFormat,
                                        defaultDecompressParams)


import Data.ByteString
import Data.Either
import Data.IORef
import Prelude

decompress :: Transform ByteString (Either DecompressError ByteString)
decompress =
  Transform $ \oldFetch -> do
    initChunksRef <- newIORef []
    decompRef <- newIORef (decompressIO
                           gzipFormat
                           defaultDecompressParams)
    return (runGzip initChunksRef decompRef oldFetch)

