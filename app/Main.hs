module Main where

import Potoki.Zlib.Transform (decompress)

import Potoki.IO (produceAndConsume)
import Potoki.Produce (transform, fileBytes)
import Potoki.Consume (printBytes, writeBytesToFile)

import OptparseApplicative.Simple.IO (parser)
import OptparseApplicative.Simple.Parser (lenientArgument)

import Potoki.Zlib.Prelude

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

main = do
  Args file _port <- parseArgs
  putStrLn $ "Decompressing " <> file <> " ..."
  produceAndConsume (transform (right' decompress) (fileBytes file))
                    --(right' (right' printBytes))
                    (right' (right' (writeBytesToFile "output")))

-- | Potoki example usages:

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

