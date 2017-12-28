module Main (main) where

main :: IO ()
main = do
    input <- map read . lines <$> getContents :: IO [Double]

    print $ sum input / fromIntegral (length input)
