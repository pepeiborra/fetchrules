{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, PatternGuards #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}
module TRS.FetchRules.OBJ where

import Data.List (elemIndex, nub, partition)
import Data.Generics
import TRS
import TRS.FetchRules
import OBJ_parser as OBJ (OBJProgram(..), Term(..), EqD(..), TermD, St,object,anyTerm)

data OBJParser = OBJParser OBJProgram
objParser = proxy :: Proxy OBJProgram

instance (HashConsed f, Var :<: f, T String :<: f) => ParseProgram OBJParser OBJ.St f where
   extension _ = "maude"
   programP = do
      prog <- OBJ.object
      return (fetchRules prog, OBJParser prog)
   termP (OBJParser prog) = fetchTerm prog `fmap` OBJ.anyTerm

fetchRules (Obj _ _ _ obj_vars obj_eqs) = map fromOBJEq obj_eqs
      where fromOBJTerm (OBJ.Var sym) | Just i <- elemIndex sym vars = varLabeled sym i
            fromOBJTerm (Call f tt) = term f (map fromOBJTerm tt) 
            vars = [ name | (name,_sort) <- obj_vars ]
            fromOBJEq (Eq t1 t2) = fromOBJTerm t1 :-> fromOBJTerm t2
fetchTerm  (Obj _ _ _ obj_vars _obj_eqs) = fromOBJTerm
      where fromOBJTerm (OBJ.Var sym) | Just i <- elemIndex sym vars = varLabeled sym i
            fromOBJTerm (Call f tt) = term f (map fromOBJTerm tt) 
            vars = [ name | (name,_sort) <- obj_vars ]
            fromOBJEq (Eq t1 t2) = fromOBJTerm t1 :-> fromOBJTerm t2
