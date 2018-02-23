{-# LANGUAGE DeriveFunctor #-}
import Control.Applicative
import Control.Monad

data Parser a = Parser { unParser :: String -> Maybe (a, String) }
  deriving (Functor)

instance Monad Parser where
  return a = Parser $ \s -> Just (a,s)
  a >>= f = Parser $ \s -> case unParser a s of
                             Nothing -> Nothing
                             Just (result, rest) -> (unParser $ f result) rest

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  a <|> b = Parser $ \s -> case unParser a s of
                             Nothing -> unParser b s
                             Just a' -> Just a'

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance MonadPlus Parser
