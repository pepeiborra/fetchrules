{-# LANGUAGE MultiParamTypeClasses, PatternGuards #-}
module TRS.FetchRules.TRS where

import Data.List (elemIndex, nub, partition)
import Data.Generics
import TRS
import TRS.FetchRules
import qualified TRSTypes as TRST
import qualified TRSParser as TRSP

trsParser = proxy :: TRST.Spec

instance ParseProgram  TRST.Spec TRST.Term () where
   grammar _ = "trs"
   programP  = TRSP.trsParser
   termP     = TRSP.term
   needsProgramToParseTerm _ = False

instance FetchRules TRST.Spec TRST.Term where
  fetchRules (TRST.Spec decls) = everything (++) (mkQ [] f) decls
   where f (lhs TRST.:-> rhs) = [mkTerm lhs :-> mkTerm rhs]
         vars = concat [v | TRST.Var v <- decls]
         mkTerm (TRST.T id tt) 
             | Just i <- id `elemIndex` vars = varLabeled id i
             | otherwise      = term id (mkTerm `map` tt)
  fetchTerm_ (TRST.Spec decls) = mkTerm
   where vars = concat [v | TRST.Var v <- decls]
         mkTerm (TRST.T id tt) 
             | Just i <- id `elemIndex` vars = varLabeled id i
             | otherwise      = term id (mkTerm `map` tt)
