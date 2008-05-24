{-# LANGUAGE TypeOperators, PolymorphicComponents, PatternSignatures, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, TypeSynonymInstances #-}
module TRS.FetchRules (parseFile, parseFileAndTerms, ParseProgram(..), FetchRules(..), parsers) where

import Control.Applicative
import Control.Monad.Error
import Data.AlaCarte
import Data.List (elemIndex, nub, partition)
import Data.Generics
import Data.Maybe
import System.FilePath
import Text.ParserCombinators.Parsec

import TRS

import OBJ_parser as OBJ (OBJProgram(..), Term(..), EqD(..), TermD, parseOBJ, parseTerm)
import qualified TTTParser as TTT
import qualified TRSParser as TRSP
import qualified TRSTypes as TRST


parseFile :: (Var :<: t, T :<: t) =>
             [Char] -> String -> Either ParseError [Rule t]
parseFile fn contents = msum(map tryParser guessedParsers) `mplus`
                   tryParser (head guessedParsers)  -- For error displaying
    where tryParser (SomeParser p) = fmap fetchRules (parseP p fn contents)
          guessedParsers = sortParsers fn

parseFileAndTerms :: (Var :<: t, T :<: t) =>
             [Char] -> String -> [String] -> Either ParseError ([Rule t ], [TRS.Term t])
parseFileAndTerms fn contents terms =
           msum(map tryParser guessedParsers) `mplus`
           tryParser (head guessedParsers)  -- For error displaying
    where tryParser (SomeParser p) = do
            program <- parseP p fn contents
            terms   <- mapM (parseT p contents) terms
            return (fetchRules program, fetchTerm program <$> terms)
          guessedParsers = sortParsers fn

sortParsers :: FilePath -> [SomeParser]
sortParsers fn = let (yes, no) = partition (\(SomeParser p) -> ext == grammar p) parsers in yes ++ no
    where ext = takeExtension fn

--------------------------------------
type Proxy a = a
proxy = undefined :: Proxy a

type FullProgram = String

class ParseProgram program term error | program -> term error
                                      , term    -> program error
 where   grammar      :: Proxy program  -> String
         parseProgram :: FilePath -> FullProgram -> Either error program
         parseTerm    :: FullProgram  -> String -> Either error term
         needsProgramToParseTerm :: Proxy program -> Bool

parseP :: ParseProgram program t e => Proxy program -> (FilePath -> FullProgram -> Either e program)
parseP _ = parseProgram

parseT :: ParseProgram p t e => Proxy p -> (FullProgram -> String -> Either e t)
parseT _ = TRS.FetchRules.parseTerm

instance ParseProgram TTT.TRS TTT.Term ParseError where
   grammar _    = "ttt"
   parseProgram = parse TTT.tttParser
   parseTerm _  = parse TTT.term "<expr>"
   needsProgramToParseTerm _ = False

instance ParseProgram OBJProgram TermD ParseError where
   grammar _      = "maude"
   parseProgram _ = parseOBJ
   parseTerm      = OBJ.parseTerm
   needsProgramToParseTerm _ = True

instance ParseProgram  TRST.Spec TRST.Term ParseError where
   grammar _    = "trs"
   parseProgram = parse TRSP.trsParser
   parseTerm _  = parse TRSP.term "<expr>"
   needsProgramToParseTerm _ = False

data SomeParser where
  SomeParser ::  forall p t. (ParseProgram p t ParseError, FetchRules p t) => Proxy p -> SomeParser

parsers = [SomeParser (proxy :: Proxy TTT.TRS)
          ,SomeParser (proxy :: Proxy OBJProgram)
          ,SomeParser (proxy :: Proxy TRST.Spec)]

class FetchRules program term | program -> term, term -> program where
  fetchRules :: (Var :<: t, T :<: t) => program -> [Rule t]
  fetchTerm  :: (Var :<: t, T :<: t) => program -> term -> TRS.Term t

instance FetchRules OBJProgram TermD where
  fetchRules (Obj _ _ _ obj_vars obj_eqs) = map fromOBJEq obj_eqs
      where fromOBJTerm (OBJ.Var sym) | Just i <- elemIndex sym vars = var i
            fromOBJTerm (Call f tt) = term f (map fromOBJTerm tt) 
            vars = [ name | (name,_sort) <- obj_vars ]
            fromOBJEq (Eq t1 t2) = fromOBJTerm t1 :-> fromOBJTerm t2
  fetchTerm (Obj _ _ _ obj_vars _obj_eqs) = fromOBJTerm
      where fromOBJTerm (OBJ.Var sym) | Just i <- elemIndex sym vars = var i
            fromOBJTerm (Call f tt) = term f (map fromOBJTerm tt) 
            vars = [ name | (name,_sort) <- obj_vars ]
            fromOBJEq (Eq t1 t2) = fromOBJTerm t1 :-> fromOBJTerm t2

instance FetchRules TTT.TRS TTT.Term where
  fetchRules rr = fmap fetchRule rr
   where fetchRule (lhs TTT.:-> rhs) = fromTerm lhs :-> fromTerm rhs
         fromTerm (TTT.V n) | Just i <- elemIndex n all_vars = var i
         fromTerm (TTT.V n) = error$ n ++  " not a Variable: perhaps you forgot to add () after a constant?"
         fromTerm (TTT.F n tt) = term n (map fromTerm tt)
         vars t = nub [ n | TTT.V n <- listify isTTTVar t]
         all_vars = nub$ concatMap vars (listify (\(_::TTT.Term)->True) rr)
         isTTTVar TTT.V{} = True
         isTTTVar _ = False
  fetchTerm rr t = termTTT t

termTTT t = fromTerm t where
         fromTerm (TTT.V n) | Just i <- elemIndex n all_vars = var i
         fromTerm (TTT.F n tt) = term n (map fromTerm tt)
         all_vars = nub [ n | TTT.V n <- listify isTTTVar t]
         isTTTVar TTT.V{} = True
         isTTTVar _ = False

instance FetchRules TRST.Spec TRST.Term where
  fetchRules (TRST.Spec decls) = everything (++) (mkQ [] f) decls
   where f (lhs TRST.:-> rhs) = [mkTerm lhs :-> mkTerm rhs]
         vars = concat [v | TRST.Var v <- decls]
         mkTerm (TRST.T id tt) 
             | Just i <- id `elemIndex` vars = var i
             | otherwise      = term id (mkTerm `map` tt)
  fetchTerm  (TRST.Spec decls) = mkTerm
   where vars = concat [v | TRST.Var v <- decls]
         mkTerm (TRST.T id tt) 
             | Just i <- id `elemIndex` vars = var i
             | otherwise      = term id (mkTerm `map` tt)

