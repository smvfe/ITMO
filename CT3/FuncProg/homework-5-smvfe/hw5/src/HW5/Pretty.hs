{-# LANGUAGE OverloadedStrings #-}

module HW5.Pretty
  ( prettyValue
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Word (Word8)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

import HW5.Base

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) = prettyNumber n
prettyValue (HiValueFunction f) = prettyFun f
prettyValue (HiValueBool True) = "true"
prettyValue (HiValueBool False) = "false"
prettyValue HiValueNull = "null"
prettyValue (HiValueString s) = prettyString s
prettyValue (HiValueList xs) = prettyHiList xs
prettyValue (HiValueBytes bs) = prettyBytes bs
prettyValue (HiValueAction a) = prettyAction a
prettyValue (HiValueTime t) = prettyTime t
prettyValue (HiValueDict m) = prettyDict m

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber n
  | denominator n == 1 = pretty (numerator n)
  | otherwise = case fromRationalRepetendUnlimited n of
      (s, Nothing) -> pretty (show s)
      _ -> prettyFraction n

prettyFraction :: Rational -> Doc AnsiStyle
prettyFraction n
  | abs num < denom = prettySimpleFrac num denom
  | otherwise = prettyMixedFrac num denom
  where
    num = numerator n
    denom = denominator n

prettySimpleFrac :: Integer -> Integer -> Doc AnsiStyle
prettySimpleFrac num denom = pretty num <> pretty ("/" :: String) <> pretty denom

prettyMixedFrac :: Integer -> Integer -> Doc AnsiStyle
prettyMixedFrac num denom =
  let (q, r) = quotRem (abs num) denom
  in if num < 0
     then pretty ("-" :: String) <> pretty q <> pretty (" - " :: String) <> pretty (abs r) <> pretty ("/" :: String) <> pretty denom
     else pretty q <> pretty (" + " :: String) <> pretty (abs r) <> pretty ("/" :: String) <> pretty denom

prettyFun :: HiFun -> Doc AnsiStyle
prettyFun HiFunDiv = "div"
prettyFun HiFunMul = "mul"
prettyFun HiFunAdd = "add"
prettyFun HiFunSub = "sub"
prettyFun HiFunNot = "not"
prettyFun HiFunAnd = "and"
prettyFun HiFunOr = "or"
prettyFun HiFunLessThan = "less-than"
prettyFun HiFunGreaterThan = "greater-than"
prettyFun HiFunEquals = "equals"
prettyFun HiFunNotLessThan = "not-less-than"
prettyFun HiFunNotGreaterThan = "not-greater-than"
prettyFun HiFunNotEquals = "not-equals"
prettyFun HiFunIf = "if"
prettyFun HiFunLength = "length"
prettyFun HiFunToUpper = "to-upper"
prettyFun HiFunToLower = "to-lower"
prettyFun HiFunReverse = "reverse"
prettyFun HiFunTrim = "trim"
prettyFun HiFunList = "list"
prettyFun HiFunRange = "range"
prettyFun HiFunFold = "fold"
prettyFun HiFunPackBytes = "pack-bytes"
prettyFun HiFunUnpackBytes = "unpack-bytes"
prettyFun HiFunEncodeUtf8 = "encode-utf8"
prettyFun HiFunDecodeUtf8 = "decode-utf8"
prettyFun HiFunZip = "zip"
prettyFun HiFunUnzip = "unzip"
prettyFun HiFunSerialise = "serialise"
prettyFun HiFunDeserialise = "deserialise"
prettyFun HiFunRead = "read"
prettyFun HiFunWrite = "write"
prettyFun HiFunMkDir = "mkdir"
prettyFun HiFunChDir = "cd"
prettyFun HiFunParseTime = "parse-time"
prettyFun HiFunRand = "rand"
prettyFun HiFunEcho = "echo"
prettyFun HiFunCount = "count"
prettyFun HiFunKeys = "keys"
prettyFun HiFunValues = "values"
prettyFun HiFunInvert = "invert"

prettyString :: Text -> Doc AnsiStyle
prettyString s = pretty (show (T.unpack s))

prettyHiList :: Seq HiValue -> Doc AnsiStyle
prettyHiList xs = case toList xs of
  [] -> "[ ]"
  elems -> "[ " <> hsep (punctuate "," (map prettyValue elems)) <> " ]"

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bs
  | BS.null bs = "[# #]"
  | otherwise = "[#" <+> hsep (map prettyByte (BS.unpack bs)) <+> "#]"

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte b = pretty (printf "%02x" b :: String)

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction HiActionCwd = "cwd"
prettyAction HiActionNow = "now"
prettyAction (HiActionRead path) = "read(" <> pretty (show path) <> ")"
prettyAction (HiActionWrite path bs) = 
  "write(" <> pretty (show path) <> ", " <> prettyBytes bs <> ")"
prettyAction (HiActionMkDir path) = "mkdir(" <> pretty (show path) <> ")"
prettyAction (HiActionChDir path) = "cd(" <> pretty (show path) <> ")"
prettyAction (HiActionRand lo hi) = "rand(" <> pretty lo <> ", " <> pretty hi <> ")"
prettyAction (HiActionEcho t) = "echo(" <> prettyString t <> ")"

prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime t = "parse-time(" <> pretty (show (show t)) <> ")"

prettyDict :: Map.Map HiValue HiValue -> Doc AnsiStyle
prettyDict m
  | Map.null m = "{ }"
  | otherwise = "{ " <> hsep (punctuate "," (map prettyEntry (Map.toList m))) <> " }"
  where
    prettyEntry (k, v) = prettyValue k <> ": " <> prettyValue v
