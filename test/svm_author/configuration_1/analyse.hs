#!/usr/bin/env runhaskell
module Main (main) where

main :: IO ()
main = do
    accuracies <- map read . lines <$> getContents

    print $ mean accuracies

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
