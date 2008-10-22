{-# LANGUAGE FlexibleContexts, TypeOperators, PolymorphicComponents, GADTs #-}
module TRS.FetchRules.Auto (SomeParser(..), parseFileAuto, parseFileAndTermsAuto, mapError, bestError) where

import Control.Applicative
import Control.Monad.Error
import Data.AlaCarte
import Data.List (elemIndex, nub, partition, maximumBy)
import Data.Monoid
import System.FilePath
import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Error
import TRS
import TRS.FetchRules

instance Error a => Error [a] where
  noMsg    = [noMsg]
  strMsg t = [strMsg t]

data SomeParser where
  SomeParser ::  forall p t s. (ParseProgram p t s, FetchRules p t) => Proxy p -> SomeParser

{- | We use an existential wrapper packaged with constraints to build the list of parsrs
parsers = [SomeParser (proxy :: Proxy TTT.TRS)
          ,SomeParser (proxy :: Proxy OBJProgram)
          ,SomeParser (proxy :: Proxy TRST.Spec)]
-}
parseFileAuto :: (Var :<: t, T String :<: t) =>
             [SomeParser] -> FilePath -> String -> Either [ParseError] [Rule t]
parseFileAuto parsers fn contents = mapError atom (msum attempts) `mplus` throwError [err | Left err <- attempts]
    where attempts = map tryParser guessedParsers
          tryParser (SomeParser p) = fmap fetchRules (parseP p fn contents)
          guessedParsers = sortParsers parsers fn

parseFileAndTermsAuto :: (Var :<: t, T String :<: t, Var :<: u, T String :<: u) =>
             [SomeParser] -> FilePath -> String -> [String] -> Either [ParseError] ([Rule t ], [TRS.Term u])
parseFileAndTermsAuto parsers fn contents terms =
           mapError atom (msum attempts) `mplus` throwError [err | Left err <- attempts]
    where attempts = map tryParser guessedParsers
          tryParser (SomeParser p) = do
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


atom x = [x]

mapError f (Left e)  = Left (f e)
mapError f (Right x) = Right x

bestError :: [ParseError] -> ParseError
bestError = maximumBy (compare `on` errorPos)
 where on cmp f x y = f x `cmp` f y