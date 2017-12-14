module Main (main) where

import qualified Control.Monad as C
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Temp as S
import qualified System.Process as S

main :: IO ()
main = S.withSystemTempFile "" $ \path h -> do
    createFeatures path
    res <- C.replicateM 100 $ runTest path
    print res
    print $ sum res / fromIntegral (length res)

createFeatures :: FilePath -> IO ()
createFeatures file = do
    (_, _, _, processHandle) <- S.createProcess (S.proc program args)
    exitCode <- S.waitForProcess processHandle

    C.unless (isSuccess exitCode) $ S.die "Feature Extraction failed."
  where
    program = "../../feature_extraction/main.py"
    args =
        [ "../../data/pan_2015/"
        , file
        , "--word-frequencies", "150"
        ]

runTest :: FilePath -> IO Double
runTest file = do
    (_, Just hout, _, ph) <- S.createProcess (S.proc program args)
        { S.std_out = S.CreatePipe }

    exitCode <- S.waitForProcess ph
    C.unless (isSuccess exitCode) $ S.die "Delta failed"

    read . init <$> S.hGetContents hout
  where
    program = "../../machine_learning/delta.py"
    args =
        [ file
        , "--with-normalization"
        , "--opposing-set-size"
        , "1"
        ]

isSuccess :: S.ExitCode -> Bool
isSuccess S.ExitSuccess = True
isSuccess _ = False
