{-# LANGUAGE MultiParamTypeClasses, PatternGuards, PatternSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}
module TRS.FetchRules.TTT where

import Data.List (elemIndex, nub, partition)
import Data.Generics
import TRS
import TRS.FetchRules
import qualified TTTParser as TTT

tttParser = proxy :: Proxy TTT.TRS

instance ParseProgram TTT.TRS TTT.Term () where
   grammar _ = "ttt"
   programP  = TTT.tttParser
   termP     = TTT.term
   needsProgramToParseTerm _ = False

instance FetchRules TTT.TRS TTT.Term where
  fetchRules rr = fmap fetchRule rr
   where fetchRule (lhs TTT.:-> rhs) = fromTerm lhs :-> fromTerm rhs
         fromTerm (TTT.V n) | Just i <- elemIndex n all_vars = varLabeled n i
         fromTerm (TTT.V n) = error$ n ++  " not a Variable: perhaps you forgot to add () after a constant?"
         fromTerm (TTT.F n tt) = term n (map fromTerm tt)
         vars t = nub [ n | TTT.V n <- listify isTTTVar t]
         all_vars = nub$ concatMap vars (listify (\(_::TTT.Term)->True) rr)
         isTTTVar TTT.V{} = True
         isTTTVar _ = False
  fetchTerm t = termTTT t
  needProgramToFetchTerm _ = False

termTTT t = fromTerm t where
         fromTerm (TTT.V n) | Just i <- elemIndex n all_vars = varLabeled n i
         fromTerm (TTT.F n tt) = term n (map fromTerm tt)
         all_vars = nub [ n | TTT.V n <- listify isTTTVar t]
         isTTTVar TTT.V{} = True
         isTTTVar _ = False
