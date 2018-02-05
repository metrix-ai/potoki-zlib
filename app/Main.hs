module Main where

import Potoki.Zlib.Prelude

import Potoki.Core.IO
import Potoki.IO (produceAndConsume)
import Potoki.Produce (transform, fileBytes)
import Potoki.Consume (printBytes)
-- import qualified Data.ByteString.Lazy as BL

-- import qualified Data.Attoparsec.Text as I
-- import qualified Data.Attoparsec.ByteString as L
-- import qualified Data.Text as J
import OptparseApplicative.Simple.IO (parser)
import OptparseApplicative.Simple.Parser (lenientArgument)

import Potoki.Zlib.Transform

data Args = Args {
    filename :: FilePath
  , ekgport  :: Int
  } deriving Show

parseArgs :: IO Args
parseArgs = parser "Gzip streaming decompressor" getargs
  where
    getargs =
      Args <$> getfile <*> getport
    getfile =
      lenientArgument
        "filename" (Just 'f') (Just "[-f filename]") Nothing
    getport =
      lenientArgument
        "ekgport" (Just 'p') (Just "[-p ekgport]") (Just (8000, "8000"))

-- main = check "large.txt.gz"
main = do
  Args file _port <- parseArgs
  produceAndConsume (transform (right' decompress) (fileBytes file))
                    (right' (right' printBytes))

-- inputs:

-- fileBytes :: FilePath -> Produce (Either IOException ByteString)
-- (fileBytes filename)

-- toChunks :: ByteString -> [!S.ByteString]
-- list :: IORef [element] -> Fetch element
-- (PP.list (BSL.toChunks lazyFile))

-- IO.produceAndConsume (Produce.transform (Profunctor.right' yourTransform)
--                                         (Produce.fileBytes yourFilePath))
--                      (Profunctor.right' (Profunctor.right'printBytes))

-- PI.produceAndConsume produced concat
-- produced = PP.transform decompress (PP.list (BSL.toChunks lazyFile))
-- consumed = PI.produceAndConsume produced

