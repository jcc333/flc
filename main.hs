module Main where
import Ast
import Eval
--import Lrt
import Parse
import Control.Monad
import Data.IORef
import System.IO
import System.Environment

main = do args <- getArgs
          case length args of
               0 -> repl emptyEnv
               otherwise -> putStrLn "flc does not accept any parameters (yet)"

parseAndPrint :: String -> IO ()
parseAndPrint s = putStrLn . show $ parseString s  

readPrompt prompt = flushStr prompt >> getLine

flushStr str = putStr str >> hFlush stdout

repl :: Env -> IO ()
repl state = readPrompt "?- " >>= \ input ->
  if input == "quit"
  then return ()
  else let stmt = parseString input 
           result = eval state stmt
       in putStrLn (show result) >> repl (env result)
