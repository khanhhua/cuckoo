module Main where

import System.Random
import CuckooLib

main :: IO ()
main = do
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

  putStrLn $ fname
    <> " " <> lname
    <> " living at " <> number
    <> " " <> streetName
    <> " " <> stateName
    <> " (" <> phone <> ") working as " <> job <> " for " <> company
