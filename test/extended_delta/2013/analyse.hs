#!/usr/bin/env runhaskell
module Main (main) where

main :: IO ()
main = do
    content <- map (map read . words) . lines <$> getContents

    let accuracies = map (\x -> x !! 0) content
        tps = map (\x -> x !! 1) content
        tns = map (\x -> x !! 2) content
        fps = map (\x -> x !! 3) content
        fns = map (\x -> x !! 4) content

        tp = mean tps
        tn = mean tns
        fp = mean fps
        fn = mean fns

    print $ ("Accuracy", "TPR", "FPR", "TNR")
    print $ (mean accuracies, tpr tp fn, fpr fp tn, tnr tn fp)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

tpr :: Double -> Double -> Double
tpr tp fn = tp / (tp + fn)

fpr :: Double -> Double -> Double
fpr fp tn = fp / (fp + tn)

tnr :: Double -> Double -> Double
tnr tn fp = tn / (tn + fp)
