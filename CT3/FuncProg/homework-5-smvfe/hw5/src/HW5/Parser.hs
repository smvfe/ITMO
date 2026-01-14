{-# LANGUAGE OverloadedStrings #-}

module HW5.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import HW5.Base

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pExpr <* eof) ""

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

pExpr :: Parser HiExpr
pExpr = makeExprParser pApply operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ InfixL (binary HiFunMul <$ symbol "*")
    , InfixL (binary HiFunDiv <$ try (symbol "/" <* notFollowedBy (char '=')))
    ]
  , [ InfixL (binary HiFunAdd <$ symbol "+")
    , InfixL (binary HiFunSub <$ symbol "-")
    ]
  , [ InfixN (binary HiFunNotGreaterThan <$ symbol "<=")
    , InfixN (binary HiFunNotLessThan <$ symbol ">=")
    , InfixN (binary HiFunLessThan <$ symbol "<")
    , InfixN (binary HiFunGreaterThan <$ symbol ">")
    , InfixN (binary HiFunEquals <$ symbol "==")
    , InfixN (binary HiFunNotEquals <$ symbol "/=")
    ]
  , [ InfixR (binary HiFunAnd <$ symbol "&&") ]
  , [ InfixR (binary HiFunOr <$ symbol "||") ]
  ]
  where
    binary :: HiFun -> HiExpr -> HiExpr -> HiExpr
    binary f a b = HiExprApply (HiExprValue (HiValueFunction f)) [a, b]

pApply :: Parser HiExpr
pApply = do
  base <- pAtom
  suffixes base
  where
    suffixes :: HiExpr -> Parser HiExpr
    suffixes e = (pArgs e >>= suffixes)
             <|> (pDot e >>= suffixes)
             <|> (pRun e >>= suffixes)
             <|> return e

    pArgs :: HiExpr -> Parser HiExpr
    pArgs e = do
      args <- parens (pExpr `sepBy` symbol ",")
      return (HiExprApply e args)

    pDot :: HiExpr -> Parser HiExpr
    pDot e = do
      _ <- symbol "."
      field <- pDotField
      return (HiExprApply e [HiExprValue (HiValueString (T.pack field))])

    pRun :: HiExpr -> Parser HiExpr
    pRun e = do
      _ <- symbol "!"
      return (HiExprRun e)

    pDotField :: Parser String
    pDotField = lexeme $ do
      first <- satisfy isAlpha
      rest <- many (satisfy (\c -> isAlphaNum c || c == '-'))
      return (first : rest)

pAtom :: Parser HiExpr
pAtom = choice
  [ parens pExpr
  , try pBytes
  , pList
  , pDict
  , HiExprValue <$> pValue
  ]

pValue :: Parser HiValue
pValue = choice
  [ pNumber
  , pBool
  , pNull
  , pString
  , pCwd
  , pNow
  , HiValueFunction <$> pFun
  ]

pNumber :: Parser HiValue
pNumber = lexeme $ do
  sign <- optional (char '-')
  num <- L.scientific
  let rational = toRational num
  return $ HiValueNumber $ case sign of
    Just _  -> negate rational
    Nothing -> rational

pBool :: Parser HiValue
pBool = HiValueBool True <$ symbol "true"
    <|> HiValueBool False <$ symbol "false"

pNull :: Parser HiValue
pNull = HiValueNull <$ symbol "null"

pCwd :: Parser HiValue
pCwd = HiValueAction HiActionCwd <$ symbol "cwd"

pNow :: Parser HiValue
pNow = HiValueAction HiActionNow <$ symbol "now"

pString :: Parser HiValue
pString = lexeme $ do
  _ <- char '"'
  str <- manyTill L.charLiteral (char '"')
  return $ HiValueString (T.pack str)

pFun :: Parser HiFun
pFun = choice
  [ HiFunDiv <$ symbol "div"
  , HiFunMul <$ symbol "mul"
  , HiFunAdd <$ symbol "add"
  , HiFunSub <$ symbol "sub"
  , HiFunNotLessThan <$ symbol "not-less-than"
  , HiFunNotGreaterThan <$ symbol "not-greater-than"
  , HiFunNotEquals <$ symbol "not-equals"
  , HiFunNot <$ symbol "not"
  , HiFunAnd <$ symbol "and"
  , HiFunOr <$ symbol "or"
  , HiFunLessThan <$ symbol "less-than"
  , HiFunGreaterThan <$ symbol "greater-than"
  , HiFunEquals <$ symbol "equals"
  , HiFunIf <$ symbol "if"
  , HiFunLength <$ symbol "length"
  , HiFunToUpper <$ symbol "to-upper"
  , HiFunToLower <$ symbol "to-lower"
  , HiFunReverse <$ symbol "reverse"
  , HiFunTrim <$ symbol "trim"
  , HiFunList <$ symbol "list"
  , HiFunRange <$ symbol "range"
  , HiFunFold <$ symbol "fold"
  , HiFunPackBytes <$ symbol "pack-bytes"
  , HiFunUnpackBytes <$ symbol "unpack-bytes"
  , HiFunEncodeUtf8 <$ symbol "encode-utf8"
  , HiFunDecodeUtf8 <$ symbol "decode-utf8"
  , HiFunZip <$ symbol "zip"
  , HiFunUnzip <$ symbol "unzip"
  , HiFunSerialise <$ symbol "serialise"
  , HiFunDeserialise <$ symbol "deserialise"
  , HiFunRead <$ symbol "read"
  , HiFunWrite <$ symbol "write"
  , HiFunMkDir <$ symbol "mkdir"
  , HiFunChDir <$ symbol "cd"
  , HiFunParseTime <$ symbol "parse-time"
  , HiFunRand <$ symbol "rand"
  , HiFunEcho <$ symbol "echo"
  , HiFunCount <$ symbol "count"
  , HiFunKeys <$ symbol "keys"
  , HiFunValues <$ symbol "values"
  , HiFunInvert <$ symbol "invert"
  ]

pList :: Parser HiExpr
pList = do
  elems <- brackets (pExpr `sepBy` symbol ",")
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) elems

pBytes :: Parser HiExpr
pBytes = lexeme $ do
  _ <- string "[#"
  sc
  bytes <- pHexByte `sepEndBy` sc
  _ <- string "#]"
  return $ HiExprValue $ HiValueBytes $ BS.pack bytes

pHexByte :: Parser Word8
pHexByte = do
  high <- hexDigitChar
  low <- hexDigitChar
  return $ fromIntegral (hexVal high * 16 + hexVal low)
  where
    hexVal :: Char -> Int
    hexVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0

pDict :: Parser HiExpr
pDict = braces $ do
  pairs <- pDictEntry `sepBy` symbol ","
  return $ HiExprDict pairs

pDictEntry :: Parser (HiExpr, HiExpr)
pDictEntry = do
  key <- pExpr
  _ <- symbol ":"
  val <- pExpr
  return (key, val)
