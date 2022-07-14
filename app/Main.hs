module Main where

import           Control.Monad.IO.Class
import qualified Data.Map                as M
    (Map, fromList)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Random

import Web.Scotty

import CuckooLib
    ( runFake, fakeFullname
    , fakeAddress, fakeJobTitle, fakeCompany
    )
import CuckooNest
    (Cuckoo(..), CuckooPairs, config, cuckooBarrage, cuckooNest)


main :: IO ()
main = scotty 3000 $ do
  get "/" randomCuckooNest

  get "/profiles" randomProfiles


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
      json $ map encodeObject nest
    Nothing -> text $ TL.pack "Bad Config"

randomProfiles = do
  g <- liftIO newStdGen
  let
    configs =
      [ ("fullname", CuckooString <$> fakeFullname)
      , ("home_address", CuckooString <$> fakeAddress)
      , ("job_title", CuckooString <$> fakeJobTitle)
      , ("current_employer", CuckooString <$> fakeCompany)
      ]
  (nest, _nextG) <- liftIO $ runFake (cuckooBarrage configs 5) g

  json $ map encodeObject nest


encodeObject :: CuckooPairs -> M.Map String Cuckoo
encodeObject = M.fromList