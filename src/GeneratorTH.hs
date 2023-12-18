{-# LANGUAGE TemplateHaskell #-}

module GeneratorTH where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Hedgehog.Internal.Gen hiding (map)
import qualified Hedgehog.Gen as Gen
import Utils
import Data.List qualified as List


conArgs :: [BangType] -> Q [Dec]
conArgs _ = pure []

consGen :: [Con] -> [Exp]
consGen = map consG

go :: BangType -> Exp
go (_, typ) 
  | typ == ConT ''Int = VarE 'defaultInt
  | typ == ConT ''Bool = VarE 'Gen.bool
  | typ == ConT ''Double = VarE 'defaultDouble
  | otherwise = error $ "Type " <> show otherwise <> " not supported"

consG :: Con -> Exp
consG (NormalC name' []) = AppE (VarE 'constant) (ConE name')
consG (NormalC name' (a : as)) = List.foldl' (AppE . AppE (VarE '(<*>))) (AppE (AppE (VarE 'fmap) (ConE name')) (go a)) $ map go as
  -- AppE $ map go as 
  -- error $ "Type " <> show name' <> " not supported"
consG ow = error $ "Type " <> show ow <> " not supported"
  -- case args of
  --       [] -> con
  --         
  --             result = pure [SigD decName (AppT (ConT ''Gen) (ConT name))] <>
  --               [d| $(varP decName) = $(conArgs args) |] in

-- support generators for
deriveGen :: Name -> Q [Dec]
deriveGen name = do
  TyConI (DataD _ name' _ _ cons _) <- reify name
  let decName = mkName $ "gen" <> nameBase name'
  pure [SigD decName (AppT (ConT ''Gen) (ConT name))] <> 
    [d| $(varP decName) = $(pure $ AppE (VarE 'Gen.choice) (ListE $ consGen cons)) |]

    -- [d| $(varP decName) = $(pure $ AppE (VarE Gen.choice) (ListE gens)) |]
--   case cons of
--     NormalC name' args : as -> conArgs args
--       case args of
--         [] -> con
--           let decName = mkName "singleGen"
--               result = pure [SigD decName (AppT (ConT ''Gen) (ConT name))] <>
--                 [d| $(varP decName) = $(conArgs args) |] in
--           -- result <> 
--         (_, bt) : bs -> pure []
--     RecC _ [(_, _, typ)] : as -> pure []
--     otherwise -> fail $ "Type " <> show otherwise <> " not supported"
--   where
--     conArgs :: [Con] -> Q [Dec] 
--     conArgs [] = constant $(conE name')