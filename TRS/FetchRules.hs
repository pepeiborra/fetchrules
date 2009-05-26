{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module TRS.FetchRules (parseFile, ParseProgram(..), Proxy, proxy) where

import Data.Monoid
import Text.ParserCombinators.Parsec (GenParser, runParser, ParseError)

import TRS

parseFile :: (ParseProgram p st f) =>
             Proxy p -> FilePath -> String -> Either ParseError [Rule f]
parseFile proxy_p fn txt = fst `fmap` ei_res where
    ei_res         = runParser programP mempty fn txt
    ~(Right (_,p)) = ei_res
    types          = proxy' p `asTypeOf` proxy_p

type FullProgram = String

class Monoid st => ParseProgram p st f | p -> st where
   extension         :: Proxy p -> String
   programP          :: GenParser Char st ([Rule f], p)
   termP             :: p -> GenParser Char st (Term f)
   parseFileAndTerms :: Proxy p -> FilePath -> String -> [String] -> Either ParseError ([Rule f], [Term f])
   parseFileAndTerms p_proxy fn contents terms = toEither $ do
            (program,p) <- fromEither $ runParser programP mempty fn contents
            let typ = proxy' p `asTypeOf` p_proxy
            terms   <- mapM (fromEither . runParser (termP p) mempty "<term>") terms
            return (program, terms)

data MyEither l r = L l | R r
fromEither = either L R
toEither (L l) = Left l; toEither (R r) = Right r

instance Monad (MyEither ParseError) where
  return = R
  L a >>= f = L a
  R r >>= f = f r

type Proxy p = p
proxy = undefined :: Proxy a

proxy' :: p -> Proxy p
proxy' _ = proxy