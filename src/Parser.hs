{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Parser (Parser, parse, item, sat, one, getN, parseWhen) where

import Control.Applicative

newtype Parser c a = Parser ([c] -> Maybe (a, [c]))

parse :: Parser c a -> [c] -> Maybe (a, [c])
parse (Parser p) s = p s

item :: Parser c c
item =
  Parser $ \case
    [] -> Nothing
    (x : xs) -> Just (x, xs)

instance Functor (Parser c) where
  fmap :: (a -> b) -> Parser c a -> Parser c b
  fmap fab (Parser p) =
    Parser $ \inp -> case p inp of
      Nothing -> Nothing
      Just (a, cs) -> Just (fab a, cs)

instance Applicative (Parser c) where
  pure :: a -> Parser c a
  pure a = Parser (\cs -> Just (a, cs))

  (<*>) :: Parser c (a -> b) -> Parser c a -> Parser c b
  (Parser f) <*> pa' = Parser $ \inp ->
    case f inp of
      Nothing -> Nothing
      Just (fab, cs) -> parse (fab <$> pa') cs

instance Monad (Parser c) where
  (>>=) :: Parser c a -> (a -> Parser c b) -> Parser c b
  (Parser g) >>= f = Parser $ \inp ->
    case g inp of
      Nothing -> Nothing
      Just (a, cs) -> parse (f a) cs

instance Alternative (Parser c) where
  empty :: Parser c a
  empty = Parser (\_ -> Nothing)

  (<|>) :: Parser c a -> Parser c a -> Parser c a
  (Parser f) <|> pa' = Parser $ \inp ->
    case f inp of
      Nothing -> parse pa' inp
      Just (a, cs) -> Just (a, cs)

instance MonadFail (Parser c) where
  fail :: String -> Parser c a
  fail _ = empty

sat :: (c -> Bool) -> Parser c c
sat p = do
  c <- item
  if p c then return c else empty

one :: Eq c => c -> Parser c c
one c = sat (== c)

getN :: Eq c => Int -> c -> Parser c [c]
getN 0 _ = return []
getN n c = do
  _ <- one c
  (c :) <$> getN (n - 1) c

parseWhen :: Alternative f => Bool -> f a -> f a
parseWhen b parser =
  if b
    then parser
    else empty
