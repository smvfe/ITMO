module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter (pretty)
import Prettyprinter.Render.Terminal (putDoc)
import System.Console.Haskeline

import HW5.Action
import HW5.Base
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty

main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  minput <- getInputLine "hi> "
  case minput of
    Nothing    -> return ()
    Just ""    -> repl
    Just input -> do
      case parse input of
        Left err -> liftIO $ putStrLn $ show err
        Right expr -> do
          result <- liftIO $ runHIO (eval expr) permissions
          case result of
            Left err  -> liftIO $ print err
            Right val -> liftIO $ do
              putDoc $ prettyValue val
              putStrLn ""
      repl

permissions :: Set HiPermission
permissions = Set.fromList [AllowRead, AllowWrite, AllowTime]
