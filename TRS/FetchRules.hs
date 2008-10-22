{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, PolymorphicComponents, KindSignatures #-}
module TRS.FetchRules (parseFile, parseFileAndTerms, ParseProgram(..), FetchRules(..), Proxy, proxy, FullProgram) where

import Control.Monad.Error
import Data.AlaCarte
import Data.Monoid
import Text.ParserCombinators.Parsec (GenParser, runParser, ParseError)

import TRS

parseFile :: (Var :<: t, T String :<: t, ParseProgram program term st, FetchRules program term) =>
             Proxy program -> FilePath -> String -> Either ParseError [Rule t]

parseFile p fn contents = fetchRules `fmap` parseP p fn contents


parseFileAndTerms :: (Var :<: t, T String :<: t, Var :<: u, T String :<: u, ParseProgram program term s, FetchRules program term) =>
             Proxy program -> FilePath -> String -> [String] -> Either ParseError ([Rule t], [Term u])

parseFileAndTerms  p fn contents terms = do
            program <- parseP p fn contents
            terms   <- mapM (parseT p contents) terms
            return (fetchRules program, fetchTerm_ program `fmap` terms)

parseP :: ParseProgram program t s => Proxy program -> (FilePath -> FullProgram -> Either ParseError program)
parseP _ = runParser programP mempty

parseT :: ParseProgram p t s => Proxy p -> (FullProgram -> String -> Either ParseError t)
parseT _ = runParser termP mempty

type FullProgram = String

class Monoid st => ParseProgram program term st | program -> term st
                                              , term    -> program st
 where   grammar  :: Proxy program  -> String
         programP :: GenParser Char st program
         termP    :: GenParser Char st term
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

instance Error ParseError