module Main where

import System.Random
import System.Random.Stateful
import Control.Monad.State.Lazy

main :: IO ()
main = do
  g <- newStdGen
  (fname, nextG) <- runFake fakeFirstName g
  (lname, _) <- runFake fakeFamilyName nextG

  putStrLn $ fname <> " " <> lname


newtype Fake a = Fake
  { runFake :: StdGen -> IO (a, StdGen)
  }

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
