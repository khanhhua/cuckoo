module CuckooNest where

import Data.Aeson
import Control.Monad.State.Lazy
import CuckooLib

data Cuckoo
  = CuckooString String
  | CuckooInt Int

instance ToJSON Cuckoo where
  toJSON (CuckooString s) = toJSON s
  toJSON (CuckooInt i) =  toJSON i

instance Show Cuckoo where
    show (CuckooString s) = show s
    show (CuckooInt i)    = show i

type Config = (String, Fake Cuckoo)

type CuckooPairs = [(String, Cuckoo)]


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


cuckooNest :: [Config] -> Fake CuckooPairs
cuckooNest configs = Fake . runFake $ sequenceA applicatives
  where
    applicatives = map asApplicative configs


cuckooBarrage :: [Config] -> Int -> Fake [CuckooPairs]
cuckooBarrage configs n = replicateM n (cuckooNest configs)
