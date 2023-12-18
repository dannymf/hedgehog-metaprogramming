{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HedgehogTest where

import Control.Monad (guard)
import Data.List qualified as List
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

prop_test_limit :: Property
prop_test_limit =
  withTests 10000 . property $
    success

prop_shrink_limit :: Property
prop_shrink_limit =
  withShrinks 0 . property $ do
    x <- forAll $ Gen.enum 'a' 'z'
    -- assert $
    --   x /= 'z'
    success

prop_foo :: Property
prop_foo =
  property $ do
    x <- forAll $ Gen.enum 'a' 'z'
    y <-
      forAll $
        Gen.choice
          [ Gen.integral (Range.linear 0 1000),
            Gen.integral (Range.linear 0 1000)
          ]

    guard (y `mod` 2 == (1 :: Int))

    diff y (<) 87
    diff x (<=) 'r'

-- Example 3

newtype Product
  = Product String
  deriving (Eq, Ord, Show)

newtype USD
  = USD Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data Item
  = Item Product USD
  deriving (Eq, Ord, Show)

newtype Order
  = Order [Item]
  deriving (Eq, Ord, Show)

merge :: Order -> Order -> Order
merge (Order xs) (Order ys) =
  Order $
    xs
      ++ ys
      ++ if any ((< 50) . price) xs
        && any ((> 50) . price) xs
        && any ((> 1050) . price) ys
        then [Item (Product "processing") (USD 1)]
        else []

price :: Item -> USD
price (Item _ x) =
  x

total :: Order -> USD
total (Order xs) =
  sum $ fmap price xs

cheap :: Gen Item
cheap =
  Item
    <$> (Product <$> Gen.element ["sandwich", "noodles"])
    <*> (USD <$> Gen.integral (Range.constant 5 10))

expensive :: Gen Item
expensive =
  Item
    <$> (Product <$> Gen.element ["oculus", "vive"])
    <*> (USD <$> Gen.integral (Range.linear 1000 2000))

order :: Gen Item -> Gen Order
order gen =
  Order <$> Gen.list (Range.linear 0 50) gen

-- | Fails with:
--
-- @
-- λ check prop_total
-- *** Failed! Falsifiable (after 4 tests and 3 shrinks):
-- cheap :: Order
-- cheap =
--   Order []
--
-- expensive :: Order
-- expensive =
--   Order [ Item (Product "oculus") (USD 1000) ]
--
-- === Not Equal ===
-- USD 1001
-- USD 1000
-- @
prop_total :: Property
prop_total =
  property $ do
    x <- forAll (order $ Gen.choice [cheap, expensive])
    y <- forAll (order expensive)
    total (merge x y) === total x + total y

-- Example 4 - Hutton's Razor

data Exp
  = Lit !Int
  | Add !Exp !Exp
  deriving (Eq, Ord, Show)

evalExp :: Exp -> Int
evalExp = \case
  Lit x ->
    x
  Add x y ->
    evalExp x + evalExp y

--
-- The subterm combinators (Gen.subterm, Gen.subtermM, Gen.subterm2,
-- Gen.subterm2M, etc) allow a generator to shrink to one of its sub-terms.
--
-- In the example below, the Add expression can shrink to either of its
-- sub-terms:
--

genExp1 :: Gen Exp
genExp1 =
  Gen.recursive
    Gen.choice
    [ Lit <$> Gen.int (Range.linear 0 10000)
    ]
    [ Gen.subterm2 genExp1 genExp1 Add
    ]

prop_hutton_1 :: Property
prop_hutton_1 =
  property $ do
    x <- forAll genExp1
    case x of
      Add (Add _ _) _ ->
        assert (evalExp x < 100)
      _ ->
        success

--
-- Gen.shrink is a more general way to add shrinks to a generator.
--
-- Here we use it to replace an expression with the literal it evaluates to:
--

shrinkExp2 :: Exp -> [Exp]
shrinkExp2 = \case
  Lit _ ->
    []
  Add x y ->
    [Lit (evalExp (Add x y))]

genExp2 :: Gen Exp
genExp2 =
  Gen.shrink shrinkExp2 $
    Gen.recursive
      Gen.choice
      [ Lit <$> Gen.int (Range.linear 0 10000)
      ]
      [ Gen.subterm2 genExp2 genExp2 Add
      ]

prop_hutton_2 :: Property
prop_hutton_2 =
  property $ do
    x <- forAll genExp2
    case x of
      Add (Add _ _) _ ->
        assert (evalExp x < 100)
      _ ->
        success

------------------------------------------------------------------------
-- Example 5 - Diff Record

data SomeRecord = SomeRecord
  { someInt :: Int,
    someBool :: Bool,
    someDouble :: Double,
    someList :: [(Int, String)]
  }
  deriving (Eq, Show)

genRecord :: Gen SomeRecord
genRecord =
  SomeRecord
    <$> Gen.int (Range.linearFrom 0 (-1000) 1000)
    <*> Gen.bool
    <*> Gen.double (Range.linearFrac 7.2 15.9)
    <*> Gen.list
      (Range.linear 5 100)
      ( (,)
          <$> Gen.int (Range.constant 0 10)
          <*> Gen.string (Range.constant 2 4) Gen.alpha
      )

prop_record :: Property
prop_record =
  property $ do
    x <- forAll genRecord
    y <- forAll genRecord
    diff x (==) y

prop_different_record :: Property
prop_different_record =
  property $ do
    x <- forAll genRecord
    x /== x

------------------------------------------------------------------------
-- Example 6 - Text.takeEnd

prop_takeEnd :: Property
prop_takeEnd =
  property $ do
    xs <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
    n <- forAll $ Gen.int (Range.linear 0 100)

    let string =
          List.reverse . List.take n $ List.reverse xs

        text =
          Text.unpack . Text.takeEnd n $ Text.pack xs

    string === text

tests :: IO Bool
tests =
  -- checkSequential $
  --   Group
  --     "HedgehogTest"
  --     [ ("prop_total", prop_total)
  --     ]
  checkParallel $$discover