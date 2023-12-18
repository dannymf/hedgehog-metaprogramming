{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TestGen where

import GeneratorTH
import Hedgehog

data Test2 = T1
  deriving Show

data Test3 = T3 Int Int | T4 Bool Double
  deriving Show

deriveGen ''Test3
deriveGen ''Test2

trivialTest2 :: Property
trivialTest2 = property $ do
  t2 <- forAll genTest2
  case t2 of
    T1 -> success

trivialTest3 :: Property
trivialTest3 = property $ do
  t3 <- forAll genTest3
  case t3 of
    T3 x y -> assert $ x > 0 && y > 0
    T4 _ y -> assert $ y > 0

genTests :: IO Bool
genTests =
  checkParallel $ Group "Test.Example" [
      ("trivialTest3", trivialTest3),
      ("trivialTest2", trivialTest2)
    ]