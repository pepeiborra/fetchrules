{-# LANGUAGE MultiParamTypeClasses, PatternGuards #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}
module TRS.FetchRules.OBJ where

import Data.List (elemIndex, nub, partition)
import Data.Generics
import TRS
import TRS.FetchRules
import OBJ_parser as OBJ (OBJProgram(..), Term(..), EqD(..), TermD, St,object,anyTerm)

instance ParseProgram OBJProgram TermD OBJ.St where
   grammar _ = "maude"
   programP  = OBJ.object
   termP     = OBJ.anyTerm
   needsProgramToParseTerm _ = True

instance FetchRules OBJProgram TermD where
  fetchRules (Obj _ _ _ obj_vars obj_eqs) = map fromOBJEq obj_eqs
      where fromOBJTerm (OBJ.Var sym) | Just i <- elemIndex sym vars = varLabeled sym i
            fromOBJTerm (Call f tt) = term f (map fromOBJTerm tt) 
            vars = [ name | (name,_sort) <- obj_vars ]
            fromOBJEq (Eq t1 t2) = fromOBJTerm t1 :-> fromOBJTerm t2
  fetchTerm_ (Obj _ _ _ obj_vars _obj_eqs) = fromOBJTerm
      where fromOBJTerm (OBJ.Var sym) | Just i <- elemIndex sym vars = varLabeled sym i
            fromOBJTerm (Call f tt) = term f (map fromOBJTerm tt) 
            vars = [ name | (name,_sort) <- obj_vars ]
            fromOBJEq (Eq t1 t2) = fromOBJTerm t1 :-> fromOBJTerm t2
