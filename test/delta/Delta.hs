module Main (main) where

import qualified Control.Monad as C
import qualified Data.Map as Map
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Temp as S
import qualified System.Process as S

type Accuracy = Double

type OpposingSize = Int
type DataFolder = FilePath
type WordFrequencies = Int

data Corpus = Brown | All
  deriving (Show, Eq, Ord)

data FeatureConfig = FeatureConfig DataFolder WordFrequencies Corpus
  deriving (Show, Eq, Ord)

newtype DeltaConfig = DeltaConfig OpposingSize
  deriving (Show)

data Config = Config FeatureConfig DeltaConfig
  deriving (Show)

main :: IO ()
main = do
    results <- C.mapM (uncurry testFeatures) myConfigs
    print $ zip results myConfigs
  where
    myConfigs = Map.toList . collapseFeatures $ configs

testFeatures :: FeatureConfig -> [DeltaConfig] -> IO [Accuracy]
testFeatures featureConfig deltaConfigs = S.withSystemTempFile "" $ \fp h -> do
    print ("Handling", featureConfig)
    createFeatures fp featureConfig
    mapM (testDelta fp) deltaConfigs

testDelta :: FilePath -> DeltaConfig -> IO Accuracy
testDelta infile (DeltaConfig oppositionSize) = do
    print ("Handling", DeltaConfig oppositionSize)
    results <- C.replicateM 100 $ runTest oppositionSize infile

    return $ sum results / fromIntegral (length results)

createFeatures :: FilePath -> FeatureConfig -> IO ()
createFeatures outfile (FeatureConfig infile wordFreqs corpus) = do
    (_, _, _, processHandle) <- S.createProcess (S.proc program args)
    exitCode <- S.waitForProcess processHandle

    C.unless (isSuccess exitCode) $ S.die "Feature Extraction failed."
  where
    program = "../../feature_extraction/main.py"
    args =
        [ infile
        , outfile
        , "--word-frequencies", show wordFreqs
        , "--corpus", corpusName corpus
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

corpusName :: Corpus -> String
corpusName Brown = "brown"
corpusName All = "all"

isSuccess :: S.ExitCode -> Bool
isSuccess S.ExitSuccess = True
isSuccess _ = False

configs :: [Config]
configs = Config <$> featureConfigs <*> deltaConfigs
  where
    featureConfigs = FeatureConfig <$>
        ["../../data/pan_2013", "../../data/pan_2015"] <*>
        [100, 200..500] <*>
        [Brown]
    deltaConfigs = DeltaConfig <$> [1..10]

collapseFeatures :: [Config] -> Map.Map FeatureConfig [DeltaConfig]
collapseFeatures = foldr collapse Map.empty
  where
    collapse (Config fc dc) = Map.insertWith (++) fc [dc]
