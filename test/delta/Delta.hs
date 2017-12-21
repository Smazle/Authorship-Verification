module Main (main) where

import qualified Control.Monad as C
import qualified Data.Map as Map
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Temp as S
import qualified System.Process as S
import qualified Text.PrettyPrint.Boxes as Pretty

type Accuracy = Double
type TPRate = Double
type TNRate = Double
type FPRate = Double
type FNRate = Double
type TP = Int
type TN = Int
type FP = Int
type FN = Int
type OpposingSize = Int
type DataFolder = FilePath
type WordFrequencies = Int

data Corpus = Brown | All
  deriving (Show, Eq, Ord)

data FeatureConfig = FeatureConfig DataFolder WordFrequencies Corpus
  deriving (Show, Eq, Ord)

newtype RunConfig = RunConfig OpposingSize
  deriving (Show)

data Config = Config FeatureConfig RunConfig

data Result = Result Accuracy TPRate TNRate FPRate FNRate Config

main :: IO ()
main = do
    results <- concat <$> C.mapM (uncurry testFeatures) myConfigs
    putStrLn $ formatResults results
  where
    myConfigs = Map.toList . collapseFeatures $ configs

testFeatures :: FeatureConfig -> [RunConfig] -> IO [Result]
testFeatures featureConfig runConfigs = S.withSystemTempFile "" $ \fp h -> do
    createFeatures fp featureConfig
    results <- mapM (runTests fp) runConfigs

    return $ zipWith consResult results configs
  where
    configs = map (Config featureConfig) runConfigs
    consResult (a, tp, tn, fp, fn) = Result a tp tn fp fn

runTests :: FilePath -> RunConfig ->
    IO (Accuracy, TPRate, TNRate, FPRate, FNRate)
runTests infile config = do
    results <- C.replicateM 100 $ runTest config infile

    return $ aggregate results
    {-return $ sum results / fromIntegral (length results)-}

aggregate :: [(Accuracy, TP, TN, FP, FN)] ->
    (Accuracy, Double, Double, Double, Double)
aggregate results = (mean accuracies, mean tps, mean tns, mean fps, mean fns)
  where
    accuracies = map (\(a, _, _, _, _) -> a) results
    tps = map (\(_, tp, _, _, _) -> fromIntegral tp) results
    tns = map (\(_, _, tn, _, _) -> fromIntegral tn) results
    fps = map (\(_, _, _, fp, _) -> fromIntegral fp) results
    fns = map (\(_, _, _, _, fn) -> fromIntegral fn) results

    mean xs = sum xs / fromIntegral (length xs)

runTest :: RunConfig -> FilePath -> IO (Accuracy, TP, TN, FP, FN)
runTest config file = do
    putStrLn $ unwords (program:args)

    (_, Just hout, _, ph) <- S.createProcess (S.proc program args)
        { S.std_out = S.CreatePipe }

    exitCode <- S.waitForProcess ph
    C.unless (isSuccess exitCode) $
        S.die ("Test run failed with " ++ show exitCode)

    [accuracy, tp, tn, fp, fn] <- words <$> S.hGetLine hout

    return (read accuracy, read tp, read tn, read fp, read fn)
  where
    program = "../../machine_learning/delta.py"
    args = testArgs file config

createFeatures :: FilePath -> FeatureConfig -> IO ()
createFeatures outfile config = do
    putStrLn $ unwords (program:args)

    (_, _, _, processHandle) <- S.createProcess (S.proc program args)
    exitCode <- S.waitForProcess processHandle

    C.unless (isSuccess exitCode) $
        S.die ("Feature Extraction failed with " ++ show exitCode)
  where
    program = "../../feature_extraction/main.py"
    args = featureArgs outfile config

isSuccess :: S.ExitCode -> Bool
isSuccess S.ExitSuccess = True
isSuccess _ = False

featureArgs :: FilePath -> FeatureConfig -> [String]
featureArgs filepath (FeatureConfig datafolder wordFreqs corpus) =
    datafolderArgs ++ wordFreqArgs ++ corpusArgs
  where
    datafolderArgs = [datafolder, filepath]
    wordFreqArgs = ["--word-frequencies", show wordFreqs]
    corpusArgs = ["--corpus", corpusName corpus]

    corpusName Brown = "brown"
    corpusName All = "all"

testArgs :: FilePath -> RunConfig -> [String]
testArgs filepath (RunConfig opposingSize) =
    [filepath, "--opposing-set-size", show opposingSize]

formatResults :: [Result] -> String
formatResults results =
    Pretty.render $ Pretty.vcat Pretty.top  (header:formatResults)
  where
    header = Pretty.hsep 3 Pretty.bottom $ map Pretty.text
        [ "datafolder", "word frequencies", "oppositionSize", "corpus"
        , "average 100 result" ]
    formatResults = map formatResult results

formatResult :: Result -> Pretty.Box
formatResult (Result accuracy tp tn fp fn (Config featureConfig runConfig)) =
    Pretty.hsep 3 Pretty.top $ map Pretty.text
        [ datafolder, show wordFreqs, show oppositionSize, show corpus
        , show (accuracy, tp / (tp + fn), tn / (tn + fp))
        ]
  where
    FeatureConfig datafolder wordFreqs corpus = featureConfig
    RunConfig oppositionSize = runConfig

configs :: [Config]
configs = Config <$> featureConfigs <*> runConfigs
  where
    featureConfigs = FeatureConfig <$>
        ["../../data/pan_2013", "../../data/pan_2015"] <*>
        [100, 200..500] <*>
        [Brown]
    runConfigs = RunConfig <$> [1..10]

collapseFeatures :: [Config] -> Map.Map FeatureConfig [RunConfig]
collapseFeatures = foldr collapse Map.empty
  where
    collapse (Config fc dc) = Map.insertWith (++) fc [dc]
