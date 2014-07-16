import Parser
import Typechecker
import Evaluator
import Types

import Control.Monad.Random
import Control.Monad.Trans
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
          Nothing -> return ()
          Just ":q" -> do 
            -- we're just having fun here, right?
            bye <- liftIO $ evalRandIO $ uniform goodbyes
            outputStrLn bye
          Just (':':'t':' ':input) -> do
            case parseMolecule input of
              Left (ParseError err) -> outputStrLn err
              Right expr -> do
                case typecheck expr of
                  Left (TypeError err) -> outputStrLn ("error: " ++ err)
                  Right typ -> outputStrLn (show expr ++ " : " ++ show typ)
            loop
          Just input -> do
            case parseMolecule input of
              Left (ParseError err) -> outputStrLn err
              Right expr -> case typecheck expr of
                Left (TypeError err) -> outputStrLn ("error: " ++ err)
                Right _ -> case evaluate expr of
                  Left (RuntimeError err) -> outputStrLn ("error: " ++ err)
                  Right val -> outputStrLn $ show val
            loop

goodbyes = [
    "bye!"
  , "goodbye!"
  , "see ya later!"
  , "peace!"
  , "have a nice day!"
  , "catch ya on the flip-flop!"
  ]
