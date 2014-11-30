module Main where

import           Hocker
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= hocker
