module Main where

import System.Random
import System.Random.Stateful
import Control.Monad.State.Lazy

main :: IO ()
main = do
  let 
    fullnameRandomizer = (,) <$> fakeFirstName <*> fakeFamilyName
  g <- newStdGen
  ((fname, lname), _nextG) <- runFake fullnameRandomizer g

  putStrLn $ fname <> " " <> lname


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
      names <- words <$> readFile p
      let
        (num, nextG) = uniformR (0, length names) gen
      pure (names !! num, nextG)


fakeFirstName = fakeString "data/first-names.txt"

fakeFamilyName = fakeString "data/family-names.txt"
