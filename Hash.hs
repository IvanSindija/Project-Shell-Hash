module Hash where
import Language.Exec 
import Language.Commands
import Parsing.HashParser (parseStringToTLExpr)
import qualified Data.Map as M
import System.Directory


runScript :: FilePath -> IO ()
runScript fp = do
  f <- readFile fp
  let tlexprs = parseStringToTLExpr f
  hd<-getHomeDirectory 
  runHashProgram allCommands (Left hd) tlexprs
  return ()
  
runInteractive :: IO ()
runInteractive = do
	putStrLn("Hello world i am hash")
	putStrLn("Input commands")
	hd<-getHomeDirectory 
	run (ScriptState {output = "", wd = hd, vartable = M.empty})
	
	
run :: ScriptState -> IO()
run scst= do
	putStr(">")
	f <- readLn
	if (f==":q") then return () else do 
			let tlexprs = parseStringToTLExpr f
			nScst <- runHashProgram allCommands (Right scst) tlexprs
			run nScst