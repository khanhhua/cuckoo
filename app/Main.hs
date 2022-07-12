module Main where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import System.Random
import CuckooLib

data Cuckoo
  = CuckooString String
  | CuckooInt Int
  deriving (Show)

cuckooNest :: [( String, Fake Cuckoo )] -> Fake [(String, Cuckoo)]
cuckooNest configs = Fake f
  where 
    f gen = do
      let
        attrs = map fst configs
        ms = map snd configs
      (values, nextG) <- runFake (sequenceA ms) gen
      pure (zip attrs values, nextG)

main :: IO ()
main = do
  g <- newStdGen
  let
    firstName = ( "first-name", CuckooString <$> fakeFirstName )
    lastName  = ( "last-name",  CuckooString <$> fakeFamilyName )
    email     = ( "email",      CuckooString <$> fakeEmail )

  (nest, nextG) <- runFake (cuckooNest [firstName, lastName, email]) g
  print nest

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
