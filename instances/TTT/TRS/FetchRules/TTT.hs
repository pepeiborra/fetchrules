{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module TRS.FetchRules.TTT where

import Data.List (elemIndex, nub, partition)
import Data.Generics
import TRS
import TRS.FetchRules
import qualified TTTParser as TTT

data TTTParser = TTTParser
tttParser = proxy :: Proxy TTTParser

instance (HashConsed f, Var :<: f, T String :<: f) => ParseProgram TTTParser () f where
   extension _ = "ttt"
   programP    = TTT.tttParser >>= \p -> return (fetchRules p, TTTParser)
   termP   _   = fmap fetchTerm TTT.term

fetchRules rr = fmap fetchRule rr
   where fetchRule (lhs TTT.:-> rhs) = fromTerm lhs :-> fromTerm rhs
         fromTerm (TTT.V n) | Just i <- elemIndex n all_vars = varLabeled n i
         fromTerm (TTT.V n) = error$ n ++  " not a Variable: perhaps you forgot to add () after a constant?"
         fromTerm (TTT.F n tt) = term n (map fromTerm tt)
         vars t = nub [ n | TTT.V n <- listify isTTTVar t]
         all_vars = nub$ concatMap vars (listify (\(_::TTT.Term)->True) rr)
         isTTTVar TTT.V{} = True
         isTTTVar _ = False
fetchTerm t = fromTerm t where
         fromTerm (TTT.V n) | Just i <- elemIndex n all_vars = varLabeled n i
         fromTerm (TTT.F n tt) = term n (map fromTerm tt)
         all_vars = nub [ n | TTT.V n <- listify isTTTVar t]
         isTTTVar TTT.V{} = True
         isTTTVar _ = False
