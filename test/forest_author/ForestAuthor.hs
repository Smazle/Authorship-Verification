module Main (main) where

import qualified Control.Monad as C
import qualified Data.Map as Map
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Temp as S
import qualified System.Process as S

type Accuracy = Double
type DataFolder = String
type Grams = (Int, [Int])
newtype CharacterNGrams = CharacterNGrams Grams deriving (Show, Eq, Ord)
newtype WordNGrams = WordNGrams Grams deriving (Show, Eq, Ord)
newtype PosTagNGrams = PosTagNGrams Grams deriving (Show, Eq, Ord)

data Corpus = Brown | All
  deriving (Show, Eq, Ord)

data FeatureConfig = FeatureConfig DataFolder CharacterNGrams WordNGrams
    PosTagNGrams Corpus
  deriving (Show, Eq, Ord)

newtype RunConfig = RunConfig ()
  deriving (Show)

data Config = Config FeatureConfig RunConfig

main :: IO ()
main = do
    results <- C.mapM (uncurry testFeatures) myConfigs
    print $ zip results myConfigs
  where
    myConfigs = Map.toList . collapseFeatures $ configs

testFeatures :: FeatureConfig -> [RunConfig] -> IO [Accuracy]
testFeatures featureConfig runConfigs = S.withSystemTempFile "" $ \fp h -> do
    createFeatures fp featureConfig
    mapM (testDelta fp) runConfigs

testDelta :: FilePath -> RunConfig -> IO Accuracy
testDelta infile config = do
    results <- C.replicateM 100 $ runTest config infile

    return $ sum results / fromIntegral (length results)

createFeatures :: FilePath -> FeatureConfig -> IO ()
createFeatures outfile config = do
    putStrLn $ unwords (program:args)

    (_, _, _, processHandle) <- S.createProcess (S.proc program args)
    exitCode <- S.waitForProcess processHandle

    C.unless (isSuccess exitCode) $ S.die "Feature Extraction failed."
  where
    program = "../../feature_extraction/main.py"
    args = featureArgs outfile config

runTest :: RunConfig -> FilePath -> IO Double
runTest config file = do
    putStrLn $ unwords (program:args)

    (_, Just hout, _, ph) <- S.createProcess (S.proc program args)
        { S.std_out = S.CreatePipe }

    exitCode <- S.waitForProcess ph
    C.unless (isSuccess exitCode) $ S.die "Delta failed"

    read <$> S.hGetLine hout
  where
    program = "../../machine_learning/delta.py"
    args = testArgs file config

isSuccess :: S.ExitCode -> Bool
isSuccess S.ExitSuccess = True
isSuccess _ = False

configs :: [Config]
configs = Config <$> featureConfigs <*> [RunConfig ()]
  where
    featureConfigs = FeatureConfig <$>
        ["../../data/pan_2013", "../../data/pan_2015"] <*>
        map CharacterNGrams [(500, [3, 4, 5])] <*>
        map WordNGrams [(100, [3, 4])] <*>
        map PosTagNGrams [(20, [2, 3, 4])] <*>
        [Brown]

featureArgs :: FilePath -> FeatureConfig -> [String]

featureArgs filepath (FeatureConfig datafolder cgrams wgrams pgrams corpus) =
    datafolderArgs ++ cgramsArgs ++ wgramsArgs ++ pgramsArgs ++ corpusArgs
  where
    datafolderArgs = [datafolder, filepath]
    cgramsArgs :: [String]
    cgramsArgs = ["--char-n-gram-size", show csize, "--char-n-gram"]
        ++ map show cns
    wgramsArgs = ["--word-n-gram-size", show wsize, "--word-n-gram"]
        ++ map show wns
    pgramsArgs = ["--postag-n-gram-size", show psize, "--postag-n-gram"]
        ++ map show pns
    corpusArgs = ["--corpus", corpusName corpus]

    CharacterNGrams (csize, cns) = cgrams
    WordNGrams (wsize, wns) = wgrams
    PosTagNGrams (psize, pns) = pgrams

    corpusName Brown = "brown"
    corpusName All = "all"

testArgs :: FilePath -> RunConfig -> [String]
testArgs filepath _ = [filepath, "--with-normalization"]

collapseFeatures :: [Config] -> Map.Map FeatureConfig [RunConfig]
collapseFeatures = foldr collapse Map.empty
  where
    collapse (Config fc dc) = Map.insertWith (++) fc [dc]
