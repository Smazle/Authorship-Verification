module Main (main) where

import qualified Control.Monad as C
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Temp as S
import qualified System.Process as S

type OpposingSize = Int
type DataFolder = FilePath
type WordFrequencies = Int
data Config = Config DataFolder WordFrequencies OpposingSize
  deriving (Show)

main :: IO ()
main = do
    C.mapM testDelta configs

testDelta :: Config -> IO ()
testDelta (Config dat wordFreq oppositionSize) =
    S.withSystemTempFile "" $ \path h -> do
        print (dat, wordFreq, oppositionSize)

        createFeatures dat wordFreq path
        res <- C.replicateM 10 $ runTest oppositionSize path

        print $ sum res / fromIntegral (length res)

createFeatures :: FilePath -> Int -> FilePath -> IO ()
createFeatures infile wordFreqs outfile = do
    (_, _, _, processHandle) <- S.createProcess (S.proc program args)
    exitCode <- S.waitForProcess processHandle

    C.unless (isSuccess exitCode) $ S.die "Feature Extraction failed."
  where
    program = "../../feature_extraction/main.py"
    args =
        [ infile
        , outfile
        , "--word-frequencies", show wordFreqs
        ]

runTest :: Int -> FilePath -> IO Double
runTest opposingSize file = do
    (_, Just hout, _, ph) <- S.createProcess (S.proc program args)
        { S.std_out = S.CreatePipe }

    exitCode <- S.waitForProcess ph
    C.unless (isSuccess exitCode) $ S.die "Delta failed"

    read <$> S.hGetLine hout
  where
    program = "../../machine_learning/delta.py"
    args =
        [ file
        , "--with-normalization"
        , "--opposing-set-size", show opposingSize
        ]

isSuccess :: S.ExitCode -> Bool
isSuccess S.ExitSuccess = True
isSuccess _ = False

configs :: [Config]
configs = Config <$> ["../../data/pan_2013", "../../data/pan_2015"]
    <*> [100, 150..300]
    <*> [1..10]
