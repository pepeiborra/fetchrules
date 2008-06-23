{-# LANGUAGE FlexibleContexts, TypeOperators, PolymorphicComponents, GADTs #-}
module TRS.FetchRules.Auto (SomeParser(..), parseFile, parseFileAndTerms) where

import Control.Applicative
import Control.Monad.Error
import Data.AlaCarte
import Data.List (elemIndex, nub, partition)
import Data.Monoid
import System.FilePath
import Text.ParserCombinators.Parsec hiding (parse)
import TRS
import TRS.FetchRules


data SomeParser where
  SomeParser ::  forall p t s. (ParseProgram p t s, FetchRules p t) => Proxy p -> SomeParser

{- | We use an existential wrapper packaged with constraints to build the list of parsrs
parsers = [SomeParser (proxy :: Proxy TTT.TRS)
          ,SomeParser (proxy :: Proxy OBJProgram)
          ,SomeParser (proxy :: Proxy TRST.Spec)]
-}
parseFile :: (Var :<: t, T String :<: t) =>
             [SomeParser] -> FilePath -> String -> Either ParseError [Rule t]
parseFile parsers fn contents = msum(map tryParser guessedParsers) `mplus`
                   tryParser (head guessedParsers)  -- For error displaying
    where tryParser (SomeParser p) = fmap fetchRules (parseP p fn contents)
          guessedParsers = sortParsers parsers fn

parseFileAndTerms :: (Var :<: t, T String :<: t) =>
             [SomeParser] -> FilePath -> String -> [String] -> Either ParseError ([Rule t ], [TRS.Term t])
parseFileAndTerms parsers fn contents terms =
           msum(map tryParser guessedParsers) `mplus`
           tryParser (head guessedParsers)  -- For error displaying
    where tryParser (SomeParser p) = do
            program <- parseP p fn contents
            terms   <- mapM (parseT p contents) terms
            return (fetchRules program, fetchTerm_ program <$> terms)
          guessedParsers = sortParsers parsers fn

sortParsers :: [SomeParser] -> FilePath -> [SomeParser]
sortParsers parsers fn = let (yes, no) = partition (\(SomeParser p) -> ext == grammar p) parsers in yes ++ no
    where ext = takeExtension fn


parseP :: ParseProgram program t s => Proxy program -> (FilePath -> FullProgram -> Either ParseError program)
parseP _ = parse programP

parseT :: ParseProgram p t s => Proxy p -> (FullProgram -> String -> Either ParseError t)
parseT _ = parse TRS.FetchRules.termP

parse p = runParser p mempty


instance Error ParseError