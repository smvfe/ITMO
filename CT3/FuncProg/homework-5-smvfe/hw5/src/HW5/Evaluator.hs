{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (bestCompression, compressWith, decompress,
                                defaultCompressParams, compressLevel)
import Codec.Serialise (deserialise, serialise)
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word8)
import Text.Read (readMaybe)

import HW5.Base

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalExpr expr)

evalExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExpr (HiExprValue v) = return v
evalExpr (HiExprApply f args) = do
  fVal <- evalExpr f
  applyFunc fVal args
evalExpr (HiExprRun e) = do
  v <- evalExpr e
  case v of
    HiValueAction a -> lift (runAction a)
    _               -> throwError HiErrorInvalidArgument
evalExpr (HiExprDict pairs) = do
  evaluatedPairs <- mapM (\(k, v) -> (,) <$> evalExpr k <*> evalExpr v) pairs
  return $ HiValueDict $ Map.fromList evaluatedPairs

applyFunc :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyFunc (HiValueFunction f) args = applyHiFun f args
applyFunc (HiValueString s) args = applyString s args
applyFunc (HiValueList xs) args = applyList xs args
applyFunc (HiValueBytes bs) args = applyBytes bs args
applyFunc (HiValueDict m) args = applyDict m args
applyFunc _ _ = throwError HiErrorInvalidFunction

applyHiFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
applyHiFun HiFunIf args = case args of
  [cond, t, f] -> do
    condVal <- evalExpr cond
    case condVal of
      HiValueBool True  -> evalExpr t
      HiValueBool False -> evalExpr f
      _                 -> throwError HiErrorInvalidArgument
  _ -> throwError HiErrorArityMismatch

applyHiFun HiFunAnd args = case args of
  [a, b] -> do
    aVal <- evalExpr a
    case aVal of
      HiValueNull       -> return HiValueNull
      HiValueBool False -> return (HiValueBool False)
      _                 -> evalExpr b
  _ -> throwError HiErrorArityMismatch

applyHiFun HiFunOr args = case args of
  [a, b] -> do
    aVal <- evalExpr a
    case aVal of
      HiValueNull       -> evalExpr b
      HiValueBool False -> evalExpr b
      _                 -> return aVal
  _ -> throwError HiErrorArityMismatch

applyHiFun f args = do
  evalArgs <- mapM evalExpr args
  applyStrict f evalArgs

applyStrict :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
applyStrict HiFunAdd [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a + b)
applyStrict HiFunAdd [HiValueString a, HiValueString b] = return $ HiValueString (a <> b)
applyStrict HiFunAdd [HiValueList a, HiValueList b] = return $ HiValueList (a >< b)
applyStrict HiFunAdd [HiValueBytes a, HiValueBytes b] = return $ HiValueBytes (a <> b)
applyStrict HiFunAdd [HiValueTime t, HiValueNumber n] = 
  return $ HiValueTime $ addUTCTime (fromRational n) t

applyStrict HiFunSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a - b)
applyStrict HiFunSub [HiValueTime a, HiValueTime b] = 
  return $ HiValueNumber $ toRational $ diffUTCTime a b

applyStrict HiFunMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a * b)
applyStrict HiFunMul [HiValueString s, HiValueNumber n] = stimesValue s n HiValueString
applyStrict HiFunMul [HiValueList xs, HiValueNumber n] = stimesValue xs n HiValueList
applyStrict HiFunMul [HiValueBytes bs, HiValueNumber n] = stimesValue bs n HiValueBytes

applyStrict HiFunDiv [HiValueNumber _, HiValueNumber 0] = throwError HiErrorDivideByZero
applyStrict HiFunDiv [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a / b)
applyStrict HiFunDiv [HiValueString a, HiValueString b] = 
  return $ HiValueString (a <> "/" <> b)

applyStrict HiFunNot [HiValueBool b] = return $ HiValueBool (not b)

applyStrict HiFunEquals [a, b] = return $ HiValueBool (a == b)
applyStrict HiFunNotEquals [a, b] = return $ HiValueBool (a /= b)
applyStrict HiFunLessThan [a, b] = return $ HiValueBool (a < b)
applyStrict HiFunGreaterThan [a, b] = return $ HiValueBool (a > b)
applyStrict HiFunNotLessThan [a, b] = return $ HiValueBool (a >= b)
applyStrict HiFunNotGreaterThan [a, b] = return $ HiValueBool (a <= b)

applyStrict HiFunLength [HiValueString s] = return $ HiValueNumber $ fromIntegral $ T.length s
applyStrict HiFunLength [HiValueList xs] = return $ HiValueNumber $ fromIntegral $ Seq.length xs
applyStrict HiFunLength [HiValueBytes bs] = return $ HiValueNumber $ fromIntegral $ BS.length bs

applyStrict HiFunToUpper [HiValueString s] = return $ HiValueString $ T.toUpper s
applyStrict HiFunToLower [HiValueString s] = return $ HiValueString $ T.toLower s
applyStrict HiFunReverse [HiValueString s] = return $ HiValueString $ T.reverse s
applyStrict HiFunReverse [HiValueList xs] = return $ HiValueList $ Seq.reverse xs
applyStrict HiFunReverse [HiValueBytes bs] = return $ HiValueBytes $ BS.reverse bs
applyStrict HiFunTrim [HiValueString s] = return $ HiValueString $ T.strip s

applyStrict HiFunList args = return $ HiValueList $ Seq.fromList args

applyStrict HiFunRange [HiValueNumber a, HiValueNumber b] = 
  return $ HiValueList $ Seq.fromList $ map HiValueNumber [a .. b]

applyStrict HiFunFold [_, HiValueList xs] | Seq.null xs = return HiValueNull
applyStrict HiFunFold [HiValueFunction f, HiValueList xs] = 
  foldl1M (\a b -> applyStrict f [a, b]) (toList xs)
  where
    foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
    foldl1M _ []     = error "foldl1M: empty list"
    foldl1M g (x:ys) = foldM g x ys

applyStrict HiFunPackBytes [HiValueList xs] = do
  bytes <- mapM toWord8 (toList xs)
  return $ HiValueBytes $ BS.pack bytes
  where
    toWord8 (HiValueNumber n)
      | denominator n == 1 && num >= 0 && num <= 255 = return (fromIntegral num)
      | otherwise = throwError HiErrorInvalidArgument
      where num = numerator n
    toWord8 _ = throwError HiErrorInvalidArgument

applyStrict HiFunUnpackBytes [HiValueBytes bs] = 
  return $ HiValueList $ Seq.fromList $ map (HiValueNumber . fromIntegral) $ BS.unpack bs

applyStrict HiFunEncodeUtf8 [HiValueString s] = return $ HiValueBytes $ TE.encodeUtf8 s

applyStrict HiFunDecodeUtf8 [HiValueBytes bs] = 
  case TE.decodeUtf8' bs of
    Left _  -> return HiValueNull
    Right s -> return $ HiValueString s

applyStrict HiFunZip [HiValueBytes bs] = 
  return $ HiValueBytes $ LBS.toStrict $ 
    compressWith defaultCompressParams { compressLevel = bestCompression } $ LBS.fromStrict bs

applyStrict HiFunUnzip [HiValueBytes bs] = 
  return $ HiValueBytes $ LBS.toStrict $ decompress $ LBS.fromStrict bs

applyStrict HiFunSerialise [v] = return $ HiValueBytes $ LBS.toStrict $ serialise v

applyStrict HiFunDeserialise [HiValueBytes bs] = 
  return $ deserialise $ LBS.fromStrict bs

applyStrict HiFunRead [HiValueString s] = 
  return $ HiValueAction $ HiActionRead $ T.unpack s

applyStrict HiFunWrite [HiValueString path, HiValueString content] = 
  return $ HiValueAction $ HiActionWrite (T.unpack path) (TE.encodeUtf8 content)
applyStrict HiFunWrite [HiValueString path, HiValueBytes content] = 
  return $ HiValueAction $ HiActionWrite (T.unpack path) content

applyStrict HiFunMkDir [HiValueString s] = 
  return $ HiValueAction $ HiActionMkDir $ T.unpack s

applyStrict HiFunChDir [HiValueString s] = 
  return $ HiValueAction $ HiActionChDir $ T.unpack s

applyStrict HiFunParseTime [HiValueString s] = 
  case readMaybe (T.unpack s) of
    Just t  -> return $ HiValueTime t
    Nothing -> return HiValueNull

applyStrict HiFunRand [HiValueNumber a, HiValueNumber b]
  | denominator a == 1 && denominator b == 1 = 
      return $ HiValueAction $ HiActionRand (fromInteger $ numerator a) (fromInteger $ numerator b)
  | otherwise = throwError HiErrorInvalidArgument

applyStrict HiFunEcho [HiValueString s] = return $ HiValueAction $ HiActionEcho s

applyStrict HiFunCount [HiValueString s] = 
  return $ HiValueDict $ Map.map HiValueNumber $ 
    Map.fromListWith (+) [(HiValueString (T.singleton c), 1) | c <- T.unpack s]
applyStrict HiFunCount [HiValueBytes bs] = 
  return $ HiValueDict $ Map.map HiValueNumber $ 
    Map.fromListWith (+) [(HiValueNumber (fromIntegral b), 1) | b <- BS.unpack bs]
applyStrict HiFunCount [HiValueList xs] = 
  return $ HiValueDict $ Map.map HiValueNumber $ 
    Map.fromListWith (+) [(x, 1) | x <- toList xs]

applyStrict HiFunKeys [HiValueDict m] = 
  return $ HiValueList $ Seq.fromList $ Map.keys m

applyStrict HiFunValues [HiValueDict m] = 
  return $ HiValueList $ Seq.fromList $ Map.elems m

applyStrict HiFunInvert [HiValueDict m] = 
  return $ HiValueDict $ Map.map (HiValueList . Seq.fromList) $
    Map.fromListWith (++) [(v, [k]) | (k, v) <- Map.toList m]

-- Catch-all: check arity first, then argument types
applyStrict f args
  | length args /= expectedArity f = throwError HiErrorArityMismatch
  | otherwise = throwError HiErrorInvalidArgument

-- | Expected arity for each function
expectedArity :: HiFun -> Int
expectedArity HiFunNot = 1
expectedArity HiFunLength = 1
expectedArity HiFunToUpper = 1
expectedArity HiFunToLower = 1
expectedArity HiFunReverse = 1
expectedArity HiFunTrim = 1
expectedArity HiFunPackBytes = 1
expectedArity HiFunUnpackBytes = 1
expectedArity HiFunEncodeUtf8 = 1
expectedArity HiFunDecodeUtf8 = 1
expectedArity HiFunZip = 1
expectedArity HiFunUnzip = 1
expectedArity HiFunSerialise = 1
expectedArity HiFunDeserialise = 1
expectedArity HiFunRead = 1
expectedArity HiFunMkDir = 1
expectedArity HiFunChDir = 1
expectedArity HiFunParseTime = 1
expectedArity HiFunEcho = 1
expectedArity HiFunCount = 1
expectedArity HiFunKeys = 1
expectedArity HiFunValues = 1
expectedArity HiFunInvert = 1
expectedArity HiFunAdd = 2
expectedArity HiFunSub = 2
expectedArity HiFunMul = 2
expectedArity HiFunDiv = 2
expectedArity HiFunAnd = 2
expectedArity HiFunOr = 2
expectedArity HiFunLessThan = 2
expectedArity HiFunGreaterThan = 2
expectedArity HiFunEquals = 2
expectedArity HiFunNotLessThan = 2
expectedArity HiFunNotGreaterThan = 2
expectedArity HiFunNotEquals = 2
expectedArity HiFunRange = 2
expectedArity HiFunFold = 2
expectedArity HiFunWrite = 2
expectedArity HiFunRand = 2
expectedArity HiFunIf = 3
expectedArity HiFunList = -1  -- variadic

stimesValue :: (Semigroup a, HiMonad m) => a -> Rational -> (a -> HiValue) -> ExceptT HiError m HiValue
stimesValue _ n _ | n <= 0 = throwError HiErrorInvalidArgument
stimesValue v n wrap
  | denominator n /= 1 = throwError HiErrorInvalidArgument
  | otherwise = return $ wrap $ stimes (numerator n) v

applyString :: HiMonad m => Text -> [HiExpr] -> ExceptT HiError m HiValue
applyString s args = do
  evalArgs <- mapM evalExpr args
  case evalArgs of
    [HiValueNumber i] -> stringIndex s i
    [a, b] -> stringSlice s a b
    _ -> throwError HiErrorArityMismatch

stringIndex :: HiMonad m => Text -> Rational -> ExceptT HiError m HiValue
stringIndex s i
  | denominator i /= 1 = throwError HiErrorInvalidArgument
  | idx < 0 || idx >= T.length s = return HiValueNull
  | otherwise = return $ HiValueString $ T.singleton $ T.index s idx
  where idx = fromInteger (numerator i)

stringSlice :: HiMonad m => Text -> HiValue -> HiValue -> ExceptT HiError m HiValue
stringSlice s a b = do
  let len = T.length s
  start <- normalizeIndex len a 0
  end <- normalizeIndex len b len
  return $ HiValueString $ T.take (end - start) $ T.drop start s

applyList :: HiMonad m => Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyList xs args = do
  evalArgs <- mapM evalExpr args
  case evalArgs of
    [HiValueNumber i] -> listIndex xs i
    [a, b] -> listSlice xs a b
    _ -> throwError HiErrorArityMismatch

listIndex :: HiMonad m => Seq HiValue -> Rational -> ExceptT HiError m HiValue
listIndex xs i
  | denominator i /= 1 = throwError HiErrorInvalidArgument
  | idx < 0 || idx >= Seq.length xs = return HiValueNull
  | otherwise = return $ Seq.index xs idx
  where idx = fromInteger (numerator i)

listSlice :: HiMonad m => Seq HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
listSlice xs a b = do
  let len = Seq.length xs
  start <- normalizeIndex len a 0
  end <- normalizeIndex len b len
  return $ HiValueList $ Seq.take (end - start) $ Seq.drop start xs

applyBytes :: HiMonad m => ByteString -> [HiExpr] -> ExceptT HiError m HiValue
applyBytes bs args = do
  evalArgs <- mapM evalExpr args
  case evalArgs of
    [HiValueNumber i] -> bytesIndex bs i
    [a, b] -> bytesSlice bs a b
    _ -> throwError HiErrorArityMismatch

bytesIndex :: HiMonad m => ByteString -> Rational -> ExceptT HiError m HiValue
bytesIndex bs i
  | denominator i /= 1 = throwError HiErrorInvalidArgument
  | idx < 0 || idx >= BS.length bs = return HiValueNull
  | otherwise = return $ HiValueNumber $ fromIntegral $ BS.index bs idx
  where idx = fromInteger (numerator i)

bytesSlice :: HiMonad m => ByteString -> HiValue -> HiValue -> ExceptT HiError m HiValue
bytesSlice bs a b = do
  let len = BS.length bs
  start <- normalizeIndex len a 0
  end <- normalizeIndex len b len
  return $ HiValueBytes $ BS.take (end - start) $ BS.drop start bs

applyDict :: HiMonad m => Map.Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyDict m args = do
  evalArgs <- mapM evalExpr args
  case evalArgs of
    [k] -> return $ fromMaybe HiValueNull $ Map.lookup k m
    _ -> throwError HiErrorArityMismatch

normalizeIndex :: HiMonad m => Int -> HiValue -> Int -> ExceptT HiError m Int
normalizeIndex len HiValueNull defaultVal = return defaultVal
normalizeIndex len (HiValueNumber n) _
  | denominator n /= 1 = throwError HiErrorInvalidArgument
  | idx < 0 = return $ max 0 (len + idx)
  | otherwise = return $ min len idx
  where idx = fromInteger (numerator n)
normalizeIndex _ _ _ = throwError HiErrorInvalidArgument
