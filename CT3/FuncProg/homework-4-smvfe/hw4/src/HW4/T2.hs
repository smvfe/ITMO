{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW4.T2
  ( ParseError(..)
  , Parser(..)
  , runP
  , pChar
  , parseError
  , pEof
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import Numeric.Natural (Natural)

import HW4.T1 (ExceptState(..))
import HW4.Types

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES run)) input =
  case run (0, input) of
    Error e        -> Error e
    Success (a :# _) -> Success a

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P (ES p)) <|> (P (ES q)) = P $ ES $ \st ->
    case p st of
      Success res -> Success res
      Error _     -> q st

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p = mfilter p pChar

pSkipWs :: Parser ()
pSkipWs = void $ many (pSatisfy isSpace)

pSymbol :: Char -> Parser Char
pSymbol c = pSkipWs *> pSatisfy (== c)

pDigit :: Parser Char
pDigit = pSatisfy isDigit

pNatural :: Parser String
pNatural = some pDigit

pDouble :: Parser Double
pDouble = do
  pSkipWs
  intPart <- pNatural
  fracPart <- optional $ do
    _ <- pSatisfy (== '.')
    pNatural
  let numStr = case fracPart of
        Nothing -> intPart
        Just frac -> intPart ++ "." ++ frac
  pure (read numStr)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (pExpr <* pSkipWs <* pEof)

pExpr :: Parser Expr
pExpr = do
  t <- pTerm
  pExprTail t

pExprTail :: Expr -> Parser Expr
pExprTail left = (do
    op <- pSymbol '+' <|> pSymbol '-'
    right <- pTerm
    let result = if op == '+'
                   then Op (Add left right)
                   else Op (Sub left right)
    pExprTail result)
  <|> pure left

pTerm :: Parser Expr
pTerm = do
  f <- pFactor
  pTermTail f

pTermTail :: Expr -> Parser Expr
pTermTail left = (do
    op <- pSymbol '*' <|> pSymbol '/'
    right <- pFactor
    let result = if op == '*'
                   then Op (Mul left right)
                   else Op (Div left right)
    pTermTail result)
  <|> pure left

pFactor :: Parser Expr
pFactor = pParens <|> pVal

pVal :: Parser Expr
pVal = Val <$> pDouble

pParens :: Parser Expr
pParens = do
  _ <- pSymbol '('
  e <- pExpr
  _ <- pSymbol ')'
  pure e
