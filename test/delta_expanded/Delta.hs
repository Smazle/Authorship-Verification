module Main (main) where

import qualified Control.Monad as C
import qualified Data.Map as Map
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Temp as S
import qualified System.Process as S
import qualified Text.PrettyPrint.Boxes as Pretty

type Accuracy = Double
type OpposingSize = Int
type DataFolder = FilePath
type WordFrequencies = Int
type CharacterNGram = (Int, Int)
type WordNGrams = (Int, Int)
type PosTagNGrams = (Int, Int)

data Corpus = Brown | All
  deriving (Show, Eq, Ord)

data FeatureConfig = FeatureConfig DataFolder CharacterNGram WordNGrams
    PosTagNGrams WordFrequencies Corpus
  deriving (Show, Eq, Ord)

newtype RunConfig = RunConfig OpposingSize
  deriving (Show)

data Config = Config FeatureConfig RunConfig
  deriving (Show)

data Result = Result Accuracy Config

main :: IO ()
main = do
    results <- concat <$> C.mapM (uncurry testFeatures) myConfigs
    putStrLn $ formatResults results
  where
    myConfigs = Map.toList . collapseFeatures $ configs

testFeatures :: FeatureConfig -> [RunConfig] -> IO [Result]
testFeatures featureConfig runConfigs = S.withSystemTempFile "" $ \fp h -> do
    createFeatures fp featureConfig
    accuracies <- mapM (runTests fp) runConfigs

    return $ zipWith Result accuracies configs
  where
    configs = map (Config featureConfig) runConfigs

runTests :: FilePath -> RunConfig -> IO Accuracy
runTests infile config = do
    results <- C.replicateM 100 $ runTest config infile

    return $ sum results / fromIntegral (length results)

runTest :: RunConfig -> FilePath -> IO Double
runTest config file = do
    putStrLn $ unwords (program:args)

    (_, Just hout, _, ph) <- S.createProcess (S.proc program args)
        { S.std_out = S.CreatePipe }

    exitCode <- S.waitForProcess ph
    C.unless (isSuccess exitCode) $
        S.die ("Test run failed with " ++ show exitCode)

    read <$> S.hGetLine hout
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
featureArgs filepath (FeatureConfig datafolder (cgram, cgramSize)
    (wgram, wgramSize) (pgram, pgramSize) wordFreqs corpus) =

    datafolderArgs ++ cGramArgs ++ wGramArgs ++ pGramArgs ++ wordFreqArgs ++
        corpusArgs
  where
    datafolderArgs = [datafolder, filepath]
    cGramArgs =
        ["--char-n-gram", show cgram, "--char-n-gram-size", show cgramSize]
    wGramArgs =
        ["--word-n-gram", show wgram, "--word-n-gram-size", show wgramSize]
    pGramArgs =
        ["--postag-n-gram", show pgram, "--postag-n-gram-size", show pgramSize]
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
    header = Pretty.hsep 3 Pretty.top $ map Pretty.text
        [ "datafolder", "cgrams", "cgramsize", "wgrams", "wgramsize", "pgrams"
        , "pgramsize", "word freqs", "oppositionsize", "corpus"
        , "average 100 result" ]
    formatResults = map formatResult results

formatResult :: Result -> Pretty.Box
formatResult (Result accuracy (Config featureConfig runConfig)) =
    Pretty.hsep 3 Pretty.top $ map Pretty.text
        [ datafolder, show cgrams, show cgramssize, show wgrams, show wgramssize
        , show pgrams, show pgramssize, show wordFreqs, show oppositionSize
        , show corpus, show accuracy
        ]
  where
    FeatureConfig datafolder (cgrams, cgramssize) (wgrams, wgramssize)
        (pgrams, pgramssize) wordFreqs corpus = featureConfig
    RunConfig oppositionSize = runConfig

configs :: [Config]
configs = Config <$> featureConfigs <*> runConfigs
  where
    featureConfigs = FeatureConfig <$>
        ["../../data/pan_2013", "../../data/pan_2015"] <*>
        [(3, 100), (4, 100), (5, 100)] <*>
        [(3, 100), (4, 100), (5, 100)] <*>
        [(3, 10)] <*>
        [100, 200..500] <*>
        [Brown]
    runConfigs = RunConfig <$> [1..10]

collapseFeatures :: [Config] -> Map.Map FeatureConfig [RunConfig]
collapseFeatures = foldr collapse Map.empty
  where
    collapse (Config fc dc) = Map.insertWith (++) fc [dc]
