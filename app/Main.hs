{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import System.Random
import qualified Data.Text.Lazy as TL

import Web.Scotty

import CuckooLib
    (runFake)
import CuckooNest
    (config, cuckooBarrage, cuckooNest)


main :: IO ()
main = scotty 3000 $ do
  get "/" randomCuckooNest


randomCuckooNest = do
  g <- liftIO newStdGen
  let
    maybeConfigs = config
      [ ( "customer", "fullname" )
      , ( "primary_email", "email" )
      , ( "secondary_email", "email" )
      ]
  case maybeConfigs of
    Just configs -> do
      (nest, nextG) <- liftIO $ runFake (cuckooBarrage configs 5) g
      text . TL.pack $ show nest
    Nothing -> text "Bad Config"