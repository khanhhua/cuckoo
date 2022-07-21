module CuckooNest where

import Control.Monad.State.Lazy
import CuckooLib
    ( fakeAddress,
      fakeCompany,
      fakeDomain,
      fakeEmail,
      fakeFamilyName,
      fakeFirstName,
      fakeFullname,
      fakeJobTitle,
      fakePastDate,
      fakeAnimal,
      fakeRiver,
      Cuckoo(CuckooString),
      Fake )
import Data.Map
    (Map, fromList)
import Graph
    (Graph (..))


type Config = (String, Fake Cuckoo)

type CuckooPairs = [(String, Cuckoo)]


asApplicative (attr, faker) = (attr,) <$> faker

tableOfCuckoos :: [(String, Fake Cuckoo)]
tableOfCuckoos =
  [ ( "first-name", CuckooString <$> fakeFirstName )
  , ( "family-name", CuckooString <$> fakeFamilyName )
  , ( "fullname", CuckooString <$> fakeFullname )
  , ( "email", CuckooString <$> fakeEmail )
  , ( "address", CuckooString <$> fakeAddress )
  , ( "past-date", CuckooString <$> fakePastDate )
  , ( "company", CuckooString <$> fakeCompany )
  , ( "domain", CuckooString <$> fakeDomain )
  , ( "job-title", CuckooString <$> fakeJobTitle )
  , ( "animal", CuckooString <$> fakeAnimal)
  , ( "river", CuckooString <$> fakeRiver )
  ]

lookupCuckooGen :: String -> Maybe (Fake Cuckoo)
lookupCuckooGen = flip lookup tableOfCuckoos

config :: Graph String -> Maybe (Graph (Fake Cuckoo))
config = traverse lookupCuckooGen


object :: [(String, Graph String)] -> Graph String
object = Object . fromList


{- Generate a configuration graph from string graph
-}
fromTemplate :: Graph String -> Maybe (Graph (Fake Cuckoo))
fromTemplate = traverse lookupCuckooGen
