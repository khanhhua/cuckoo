{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class
import qualified Data.Map                as M
    (Map, fromList, toList)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Random

import Web.Scotty

import CuckooLib
    (fakeAddress, fakeCompany, fakeFullname, fakeJobTitle, runFake)
import CuckooNest
    (Cuckoo (..), CuckooPairs, config, cuckooBarrage, cuckooNest)


main :: IO ()
main = scotty 3000 $ do
  post "/" customGenerator

  get "/profiles" getProfiles


customGenerator = do
  bodyAsMap :: M.Map String String <- jsonData
  g <- liftIO newStdGen
  let
    maybeConfigs = config $ M.toList bodyAsMap

  case maybeConfigs of
    Just configs -> do
      (nest, nextG) <- liftIO $ runFake (cuckooBarrage configs 5) g
      jsonEncode nest
    Nothing -> text $ TL.pack "Bad Config"

getProfiles = do
  g <- liftIO newStdGen
  let
    configs =
      [ ("fullname", CuckooString <$> fakeFullname)
      , ("home_address", CuckooString <$> fakeAddress)
      , ("job_title", CuckooString <$> fakeJobTitle)
      , ("current_employer", CuckooString <$> fakeCompany)
      ]
  (nest, _nextG) <- liftIO $ runFake (cuckooBarrage configs 5) g

  jsonEncode nest


encodeObject :: CuckooPairs -> M.Map String Cuckoo
encodeObject = M.fromList

jsonEncode = json . map encodeObject
