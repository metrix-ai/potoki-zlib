module Main where

import Potoki.Zlib.Prelude

import Potoki.Core.IO
import Potoki.IO (produceAndConsume)
import Potoki.Produce (transform, fileBytes)
import Potoki.Consume (printBytes)
-- import qualified Data.ByteString.Lazy as BL

import Potoki.Zlib.Transform

-- main = check "large.txt.gz"
main = go "small.txt.gz"

go filename = do
  produceAndConsume (transform (right' decompress) (fileBytes filename))
                    (right' (right' printBytes))

-- inputs:

-- fileBytes :: FilePath -> Produce (Either IOException ByteString)
-- (fileBytes filename)

-- toChunks :: ByteString -> [!S.ByteString]
-- list :: IORef [element] -> Fetch element
-- (PP.list (BSL.toChunks lazyFile))

-- IO.produceAndConsume (Produce.transform (Profunctor.right' yourTransform) (Produce.fileBytes yourFilePath)) (Profunctor.right' (Profunctor.right'printBytes))

-- PI.produceAndConsume produced concat
-- produced = PP.transform decompress (PP.list (BSL.toChunks lazyFile))
-- consumed = PI.produceAndConsume produced

