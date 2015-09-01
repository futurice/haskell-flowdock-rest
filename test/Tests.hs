{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy as BSL
import Data.Either
import Data.FileEmbed
import Data.Proxy
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Chat.Flowdock.REST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [exampleJsons]

exampleJsons :: TestTree
exampleJsons = testGroup "Example JSONs"
  [ p "List of organisations" $(embedStringFile "fixtures/organisations.json")      (Proxy :: Proxy [Organisation])
  , p "Organisation"          $(embedStringFile "fixtures/organisation.json")       (Proxy :: Proxy Organisation)
  , p "List of users "        $(embedStringFile "fixtures/users.json")              (Proxy :: Proxy [User])
  , p "List of org users"     $(embedStringFile "fixtures/organisation-users.json") (Proxy :: Proxy [User])
  , p "List of flows"         $(embedStringFile "fixtures/flows.json")              (Proxy :: Proxy [Flow])
  ]
  where p :: forall a. FromJSON a => String -> ByteString -> Proxy a -> TestTree
        p name bs _ = QC.testProperty name $ once $ isRight (eitherDecode bs :: Either String a) 
