module Utils where

-- import Language.Haskell.TH.Syntax
-- import Language.Haskell.TH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog

defaultInt :: Gen Int
defaultInt = Gen.integral (Range.constantFrom 500 0 1000)

defaultDouble :: Gen Double
defaultDouble = Gen.double (Range.constantFrom 500 0 1000)

defaultFloat :: Gen Float
defaultFloat = Gen.float (Range.constantFrom 500 0 1000)

defaultString :: Gen String
defaultString = Gen.string (Range.constantFrom 5 1 10) Gen.alphaNum
