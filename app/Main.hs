module Main where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import CuckooLib
    (runFake)
import CuckooNest
    (config, cuckooBarrage, cuckooNest)
import System.Random


main :: IO ()
main = do
  g <- newStdGen
  let
    maybeConfigs = config
      [ ( "customer", "fullname" )
      , ( "primary_email", "email" )
      , ( "secondary_email", "email" )
      ]
  case maybeConfigs of
    Just configs -> do
      (nest, nextG) <- runFake (cuckooBarrage configs 5) g
      print nest
    Nothing -> putStrLn "Bad Config"
