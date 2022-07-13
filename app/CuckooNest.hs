module CuckooNest where

import Control.Monad.State.Lazy
import CuckooLib

data Cuckoo
  = CuckooString String
  | CuckooInt Int

instance Show Cuckoo where
    show (CuckooString s) = show s
    show (CuckooInt i)    = show i

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

-- traverse == sequenceA . map
config :: [(String, String)] -> Maybe [(String, Fake Cuckoo)]
config = traverse (\(label, cuckooName) -> (label,) <$> lookupCuckooGen cuckooName)


cuckooNest :: [Config] -> Fake [(String, Cuckoo)]
cuckooNest configs = Fake . runFake $ sequenceA applicatives
  where
    applicatives = map asApplicative configs


cuckooBarrage :: [Config] -> Int -> Fake [[(String, Cuckoo)]]
cuckooBarrage configs n = replicateM n (cuckooNest configs)
