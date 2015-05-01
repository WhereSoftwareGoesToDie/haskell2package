module Main where

import           Anchor.Package.Process

main :: IO ()
main = extractDefaultCabalDetails >>= print
