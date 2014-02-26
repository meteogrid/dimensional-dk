{-# LANGUAGE PatternGuards #-}

module Numeric.Units.Dimensional.DK.UnitNames where

import Prelude
import Data.Monoid
import qualified Data.Map as M
import Control.Monad (liftM2)

type NameAtom = (String, String)

data UnitName = UnitName (M.Map NameAtom Int)
  deriving (Eq)

atomic :: NameAtom -> UnitName
atomic n = UnitName $ M.singleton n 1

instance Monoid UnitName where
  mempty = UnitName $ M.empty
  mappend = product'

nameOne :: UnitName
nameOne = mempty

applyPrefix :: NameAtom -> NameAtom -> NameAtom
applyPrefix (pa, pf) (ua, uf) = (pa ++ ua, pf ++ uf)

asAtom :: UnitName -> Maybe NameAtom
asAtom (UnitName m) | [(n, 1)] <- M.toList m = Just n
                    | otherwise = Nothing

type UnitNameTransformer = Maybe UnitName -> Maybe UnitName -> Maybe UnitName

noName, product, quotient :: UnitNameTransformer
noName _ _ = Nothing
product = liftM2 product'
quotient = liftM2 quotient'

product' :: UnitName -> UnitName -> UnitName
product' (UnitName m1) (UnitName m2) = UnitName $ M.filter (/= 0) $ M.unionWith (+) m1 m2

quotient' :: UnitName -> UnitName -> UnitName
quotient' n1 n2 = product' n1 (recip' n2)

recip' :: UnitName -> UnitName
recip' (UnitName m1) = UnitName $ fmap negate m1

instance Show UnitName where
  show = fullName

abbreviation :: UnitName -> String
abbreviation (UnitName m) = unwords $ map (raiseToPower fst) $ M.toList m

fullName :: UnitName -> String
fullName (UnitName m) = unwords $ map (raiseToPower snd) $ M.toList m

raiseToPower :: ((String,String) -> String) -> ((String, String), Int) -> String
raiseToPower extract (n, 0) = ""
raiseToPower extract (n, 1) = extract n
raiseToPower extract (n, p) = (extract n) ++ "^" ++ (show p)
