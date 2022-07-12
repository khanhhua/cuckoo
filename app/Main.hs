module Main where

import System.Random
import System.Random.Stateful
import Control.Monad.State.Lazy

main :: IO ()
main = do
  let 
    fullnameRandomizer = (,,,,,,,) 
      <$> fakeFirstName
      <*> fakeFamilyName
      <*> fakeHouseNumber
      <*> fakeStreetName
      <*> fakeStateName
      <*> fakePhone "+49" 9
      <*> fakeJobTitle
      <*> fakeCompany
  g <- newStdGen
  ((fname, lname, number, streetName, stateName, phone, job, company), _nextG) <- runFake fullnameRandomizer g

  putStrLn $ fname
    <> " " <> lname
    <> " living at " <> number
    <> " " <> streetName
    <> " " <> stateName
    <> " (" <> phone <> ") working as " <> job <> " for " <> company


newtype Fake a = Fake
  { runFake :: StdGen -> IO (a, StdGen)
  }

instance Functor Fake where
  fmap f (Fake randomizer) = Fake $ \g -> do
    (r, nextG) <- randomizer g
    pure (f r, nextG)

instance Applicative Fake where
  pure randomizer = undefined
  (Fake randomizer1) <*> (Fake randomizer2) = Fake $ \g -> do
    (f, nextG1) <- randomizer1 g
    (r, nextG2) <- randomizer2 nextG1
    pure (f r, nextG2)


fakeString :: FilePath -> Fake String
fakeString p = Fake f
  where
    f gen = do
      names <- lines <$> readFile p
      let
        (num, nextG) = uniformR (0, length names) gen
      pure (names !! num, nextG)

fakeHouseNumber :: Fake String
fakeHouseNumber = Fake f
  where
    f gen = do
      let
        (houseNumber, nextG) = uniformR (1 :: Int, 999) gen
      pure (show houseNumber, nextG)

fakePhone :: String -> Int -> Fake String
fakePhone prefix n = Fake f
  where
    f gen = do
      let
        replM :: State StdGen [Int]
        replM = replicateM n . state $ uniformR (0 :: Int, 9)
        (xs, nextG) = runState replM gen
      pure (prefix <> concatMap show xs, nextG)


fakeFirstName = fakeString "data/first-names.txt"

fakeFamilyName = fakeString "data/family-names.txt"

fakeStreetName = fakeString "data/street-names.txt"

fakeStateName = fakeString "data/state-names.txt"

fakeJobTitle = fakeString "data/jobs.txt"

fakeCompany = fakeString "data/companies.txt"
