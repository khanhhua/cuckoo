{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
    (replicateM)
import           Control.Monad.IO.Class
import qualified Data.Map                as M
    (Map, fromList, toList)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Random

import Web.Scotty

import CuckooLib
    (Cuckoo (..), fakeAddress, fakeCompany, fakeFullname, fakeJobTitle, runFake)
import CuckooNest
    (CuckooPairs, config, fromTemplate, object)
import Graph


main :: IO ()
main = scotty 3000 $ do
  post "/" customGenerator

  get "/profiles" getProfiles


customGenerator = do
  {-
  It could be written as follows
  ```
  config :: Maybe (Graph (Fake Cuckoo)) <- fromTemplate <$> jsonData
  ```
  -}
  template :: Graph String <- jsonData
  let
    config = fromTemplate template
  generateRandomJson config 5

getProfiles = do
  let
    template = object
      [ ("fullname", Leaf "fullname")
      , ("home_address", Leaf "address")
      , ("job_title", Leaf "job-title")
      , ("current_employer", Leaf "company")
      ]
    config = fromTemplate template
  generateRandomJson config 5


generateRandomJson config n =
  case config of
    Nothing         ->
      text $ TL.pack "Bad Config"
    Just goodConfig -> do
      g <- liftIO newStdGen
      let
        replM = replicateM n (sequenceA goodConfig)
      (nest, _nextG) <- liftIO $ runFake replM g
      json nest
