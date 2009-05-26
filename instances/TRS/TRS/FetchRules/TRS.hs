{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, PatternGuards #-}
module TRS.FetchRules.TRS where

import Data.List (elemIndex)
import Data.Generics
import TRS
import TRS.FetchRules
import qualified TRSTypes as TRST
import qualified TRSParser as TRSP

data TrsParser = TrsParser TRST.Spec

trsParser :: Proxy TrsParser
trsParser = proxy

instance (HashConsed f, Var :<: f, T String :<: f) => ParseProgram TrsParser () f where
   extension _ = "trs"
   programP  = do
      prog <- TRSP.trsParser
      return (fetchRules prog, TrsParser prog)
   termP (TrsParser prog) = fetchTerm prog `fmap` TRSP.term

fetchRules (TRST.Spec decls) = everything (++) (mkQ [] f) decls
   where f (lhs TRST.:-> rhs) = [mkTerm lhs :-> mkTerm rhs]
         vars = concat [v | TRST.Var v <- decls]
         mkTerm (TRST.T id tt)
             | Just i <- id `elemIndex` vars = varLabeled id i
             | otherwise      = term id (mkTerm `map` tt)
fetchTerm (TRST.Spec decls) = mkTerm
   where vars = concat [v | TRST.Var v <- decls]
         mkTerm (TRST.T id tt)
             | Just i <- id `elemIndex` vars = varLabeled id i
             | otherwise      = term id (mkTerm `map` tt)