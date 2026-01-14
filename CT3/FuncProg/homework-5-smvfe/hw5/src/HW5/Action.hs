{-# LANGUAGE OverloadedStrings #-}

module HW5.Action
  ( HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (randomRIO)

import HW5.Base

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException = PermissionRequired HiPermission
  deriving (Show, Eq)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f (HIO g) = HIO $ \perms -> fmap f (g perms)

instance Applicative HIO where
  pure x = HIO $ \_ -> pure x
  HIO f <*> HIO x = HIO $ \perms -> f perms <*> x perms

instance Monad HIO where
  HIO x >>= f = HIO $ \perms -> do
    a <- x perms
    let HIO g = f a
    g perms

instance HiMonad HIO where
  runAction = runHiAction

requirePermission :: HiPermission -> HIO ()
requirePermission perm = HIO $ \perms ->
  if Set.member perm perms
    then return ()
    else throwIO (PermissionRequired perm)

runHiAction :: HiAction -> HIO HiValue
runHiAction HiActionCwd = do
  requirePermission AllowRead
  HIO $ \_ -> do
    cwd <- getCurrentDirectory
    return $ HiValueString $ T.pack cwd

runHiAction (HiActionChDir path) = do
  requirePermission AllowRead
  HIO $ \_ -> do
    setCurrentDirectory path
    return HiValueNull

runHiAction (HiActionRead path) = do
  requirePermission AllowRead
  HIO $ \_ -> do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        contents <- listDirectory path
        return $ HiValueList $ Seq.fromList $ map (HiValueString . T.pack) contents
      else do
        isFile <- doesFileExist path
        if isFile
          then do
            content <- BS.readFile path
            case TE.decodeUtf8' content of
              Right text -> return $ HiValueString text
              Left _     -> return $ HiValueBytes content
          else return HiValueNull

runHiAction (HiActionWrite path content) = do
  requirePermission AllowWrite
  HIO $ \_ -> do
    BS.writeFile path content
    return HiValueNull

runHiAction (HiActionMkDir path) = do
  requirePermission AllowWrite
  HIO $ \_ -> do
    createDirectory path
    return HiValueNull

runHiAction HiActionNow = do
  requirePermission AllowTime
  HIO $ \_ -> do
    time <- getCurrentTime
    return $ HiValueTime time

runHiAction (HiActionRand lo hi) = HIO $ \_ -> do
  n <- randomRIO (lo, hi)
  return $ HiValueNumber $ fromIntegral n

runHiAction (HiActionEcho text) = do
  requirePermission AllowWrite
  HIO $ \_ -> do
    TIO.putStrLn text
    return HiValueNull
