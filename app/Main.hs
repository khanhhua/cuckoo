module Main where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import System.Random
import CuckooLib

data Cuckoo
  = CuckooString String
  | CuckooInt Int
  deriving (Show)

type Config = (String, Fake Cuckoo)

asApplicative (attr, faker) = (attr,) <$> faker

tableOfCuckoos :: [(String, Fake Cuckoo)]
tableOfCuckoos =
  [ ( "first-name", CuckooString <$> fakeFirstName )
  , ( "family-name", CuckooString <$> fakeFamilyName )
  , ( "fullname", CuckooString <$> fakeFullname )
  , ( "email", CuckooString <$> fakeEmail )
  ]

lookupCuckooGen = flip lookup tableOfCuckoos

cuckooNest :: [Config] -> Fake [(String, Cuckoo)]
cuckooNest configs = Fake f
  where 
    f gen = do
      let
        applicatives = map asApplicative configs
      (values, nextG) <- runFake (sequenceA applicatives) gen
      pure (values, nextG)

main :: IO ()
main = do
  g <- newStdGen
  let
    -- traverse == sequenceA . map
    maybeConfigs = traverse (\(label, cuckooName) -> (label,) <$> lookupCuckooGen cuckooName)
      [ ( "customer", "fullname" )
      , ( "primary_email", "email" )
      , ( "secondary_email", "email" )
      ]
  case maybeConfigs of
    Just configs -> do
      (nest, nextG) <- runFake (cuckooNest configs) g
      print nest
    Nothing -> putStrLn "Bad Config"


profile :: IO String
profile = do
  let 
    fullnameRandomizer = (,,,,,,,) 
      <$> fakeFirstName
      <*> fakeFamilyName
      <*> fakeNumber (10, 99)
      <*> fakeStreetName
      <*> fakeStateName
      <*> fakePhone "+49" 9
      <*> fakeJobTitle
      <*> fakeCompany
  g <- newStdGen
  ((fname, lname, number, streetName, stateName, phone, job, company), _nextG) <- runFake fullnameRandomizer g

  pure $ fname
    <> " " <> lname
    <> " living at " <> number
    <> " " <> streetName
    <> " " <> stateName
    <> " (" <> phone <> ") working as " <> job <> " for " <> company
