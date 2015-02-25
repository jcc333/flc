module Main where
import Ast
--import Eval
--import Lrt
import Parse
import System.IO
import System.Environment

main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOnFile $ args !! 0
               otherwise -> putStrLn "flc accepts 0 to 1 arguments"

runRepl = until_ (== "exit") (readPrompt "?- ") evalAndPrint

runOnFile f = parseFile f >>= \ stmts ->
  mapM (putStrLn . show) stmts >> return ()

evalAndPrint :: String -> IO ()
evalAndPrint s = putStrLn . show $ parseString s 

readPrompt prompt = flushStr prompt >> getLine

flushStr str = putStr str >> hFlush stdout

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
     result <- prompt
     if pred result 
        then return ()
        else action result >> until_ pred prompt action
