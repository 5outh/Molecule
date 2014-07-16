import Parser
import Typechecker
import Types

import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
          Nothing -> return ()
          Just ":q" -> return ()
          Just (':':'t':' ':input) -> do
            case parseMolecule input of
              Left (ParseError err) -> outputStrLn err
              Right expr -> do
                case typecheck expr of
                  Left (TypeError err) -> outputStrLn ("error: " ++ err)
                  Right typ -> outputStrLn (show typ)
            loop
          Just input -> do
            case parseMolecule input of
              Left (ParseError err) -> outputStrLn err
              Right expr -> outputStrLn $ show expr
            loop

                        
