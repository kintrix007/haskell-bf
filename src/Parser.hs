{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}

module Parser (Parser, parse, item) where

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
  empty = Parser (\inp -> Nothing)

  (<|>) :: Parser c a -> Parser c a -> Parser c a
  (Parser f) <|> pa' = Parser $ \inp ->
    case f inp of
      Nothing -> parse pa' inp
      Just (a, cs) -> Just (a, cs)
