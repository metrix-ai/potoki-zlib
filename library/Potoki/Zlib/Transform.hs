module Potoki.Zlib.Transform
(
  decompress
)
where

import Potoki.Zlib.Prelude
-- import qualified Potoki.Zlib.Fetch as Fetch

import Potoki.Core.Transform
-- import qualified Potoki.Core.Fetch as PF

import qualified Codec.Compression.Zlib.Internal as Z


decompress :: Transform ByteString (Either Z.DecompressError ByteString)
decompress = undefined
  -- Transform $ \ oldFetch -> do
  --   unfetchedChunksRef <- newIORef []
  --   resultRef <- newIORef B.decompressIncremental
  --   return (Fetch.zlibResult unfetchedChunksRef resultRef oldFetch)
