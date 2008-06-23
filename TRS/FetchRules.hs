{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module TRS.FetchRules (ParseProgram(..), FetchRules(..), Proxy, proxy, FullProgram) where

import Data.AlaCarte
import Data.Monoid
import Text.ParserCombinators.Parsec (GenParser)

import TRS

type FullProgram = String

class Monoid s => ParseProgram program term s | program -> term s
                                              , term    -> program s
 where   grammar  :: Proxy program  -> String
         programP :: GenParser Char s program
         termP    :: GenParser Char s term
         needsProgramToParseTerm :: Proxy program -> Bool

class FetchRules program term | program -> term, term -> program where
  fetchRules :: (Var :<: t, T String:<: t) => program -> [Rule t]
  fetchTerm_ :: (Var :<: t, T String :<: t) => program -> term -> TRS.Term t
  fetchTerm_ _ = fetchTerm
  needProgramToFetchTerm   :: Proxy program -> Bool
  needProgramToFetchTerm _ = True
  fetchTerm  ::  (Var :<: t, T String :<: t) => term -> TRS.Term t
  fetchTerm  = error "Use fetchTerm_ for this parser"



type Proxy a = a
proxy = undefined :: Proxy a
