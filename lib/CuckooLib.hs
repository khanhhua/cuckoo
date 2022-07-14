-- Reference https://two-wrongs.com/haskell-time-library-tutorial

module CuckooLib (
  Fake(..),
  fakeString,
  fakeNumber,
  fakePhone,
  fakeFirstName,
  fakeFamilyName,
  fakeFullname,
  fakeStreetName,
  fakeStateName,
  fakeAddress,
  fakeJobTitle,
  fakeCompany,
  fakeDomain,
  fakeAnimal,
  fakeRiver,
  fakeEmail,
  fakePastDate
) where

import Control.Monad.State.Lazy
import Data.Char
import Data.Time.Calendar
import Data.Time.Clock
import System.Random
import System.Random.Stateful
import Text.Printf

newtype Fake a = Fake
  { runFake :: StdGen -> IO (a, StdGen)
  }

instance Functor Fake where
  fmap f (Fake randomizer) = Fake $ \g -> do
    (r, nextG) <- randomizer g
    pure (f r, nextG)

instance Applicative Fake where
  pure a = Fake f
    where
      f gen = pure (a, gen)
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
        (num, nextG) = uniformR (0, length names - 1) gen
      pure (names !! num, nextG)

fakeNumber :: (Int, Int) -> Fake Integer
fakeNumber range = Fake f
  where
    f gen = do
      let
        (number, nextG) = uniformR range gen
      pure (toInteger number, nextG)

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

fakeFullname = (\fname lname -> fname <> " " <> lname ) <$> fakeFirstName <*> fakeFamilyName

fakeStreetName = fakeString "data/street-names.txt"

fakeStateName = fakeString "data/state-names.txt"

fakeAddress :: Fake String
fakeAddress = f <$> fakeNumber (1 :: Int, 999) <*> fakeStreetName <*> fakeStateName
  where
    f :: Integer -> String -> String -> String
    f number streetName stateName =
      printf "%d %s, %s" number streetName stateName

fakeJobTitle = fakeString "data/jobs.txt"

fakeCompany = fakeString "data/companies.txt"

fakeDomain = fakeString "data/domains.txt"

fakeAnimal = fakeString "data/animals.txt"

fakeRiver = fakeString "data/rivers.txt"

fakeEmail :: Fake String
fakeEmail = Fake f
  where
    f gen = do
      ((fname, river, domain, number), nextG) <- runFake randomizer gen
      pure (printf "%s.%s_%d@%s" (sanitize fname) (sanitize river) number domain, nextG)

    whitelist = not . (`elem` ("!@#$%^&*() " :: String))
    sanitize = map toLower . filter whitelist
    randomizer = (,,,) <$> fakeFirstName <*> fakeRiver <*> fakeDomain <*> fakeNumber (1000, 9999)

fakePastDate :: Fake String
fakePastDate = Fake f
  where
    randomizer = realToFrac <$> fakeNumber (-1577880000, 0)
    f gen = do
      currentTime <- getCurrentTime
      (diffValue, nextG) <- runFake randomizer gen
      let
        (year, month, day) = toGregorian . utctDay $ addUTCTime diffValue currentTime

      pure (printf "%d-%02d-%02d" year month day, nextG)
