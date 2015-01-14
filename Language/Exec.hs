module Language.Exec where
import Language.Expressions
import Parsing.HashParser
import Control.Applicative (pure)
import Control.Monad (when, unless, foldM)
import Data.Maybe (maybe, fromMaybe, isNothing,fromJust )
import qualified Data.Map as M
-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState
-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String
-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command
-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
, wd :: FilePath
, vartable :: VarTable
} deriving Show

unWrp ::VarTable -> Expr -> String
unWrp vt (Str s) = s
unWrp vt (Var s) = fromMaybe (error "variable " ++ s ++ " does not exist\n") $ M.lookup s vt

runCmd :: Cmd -> CommandTable -> ScriptState -> IO ScriptState
runCmd (Cmd cmdName args input out isAppend) ct scst =  do
  let getExpr = unWrp (vartable scst)
  let cmdName' = getExpr cmdName
  let cmd = fromMaybe (error ("command " ++ cmdName' ++ " does not exist\n")) $ M.lookup cmdName' ct
  fromFile <- maybe (return "") readFile (fmap getExpr input)
  let args' = args ++ parsed (betterParse (manyExpr) fromFile)
  newScst <- cmd (map getExpr args') scst
  if (isNothing out) then putStr $ output newScst++"\n" else ((if isAppend then appendFile else writeFile) (getExpr $ fromJust out) $ output newScst) 
  return newScst
  

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram _  (Left _)  []         = error "No commands to run"
runHashProgram ct (Left fp) (tle:tles) = do
  newSs <- runTopLevel ct (ScriptState {output = "", wd = fp, vartable = M.empty}) tle
  runHashProgram ct (Right newSs) tles
runHashProgram _  (Right scst) []         = return scst
runHashProgram ct (Right scst) (tle:tles) = do
  newSs <- runTopLevel ct scst tle
  runHashProgram ct (Right newSs) tles
  

  
evalComp :: ScriptState -> Comp -> Bool
evalComp scst c = let vs = unWrp (vartable scst) in case c of
             CEQ e1 e2 -> (read $ vs e1 :: Double) == (read $ vs e2 :: Double)
             CNE e1 e2 -> (read $ vs e1 :: Double) /= (read $ vs e2 :: Double)
             CLT e1 e2 -> (read $ vs e1 :: Double) <  (read $ vs e2 :: Double)
             CLE e1 e2 -> (read $ vs e1 :: Double) <= (read $ vs e2 :: Double)
             CGT e1 e2 -> (read $ vs e1 :: Double) >  (read $ vs e2 :: Double)
             CGE e1 e2 -> (read $ vs e1 :: Double) >= (read $ vs e2 :: Double)
               
             

evalPred :: ScriptState -> Pred -> Bool
evalPred scst p = case p of
             Parens p  -> evalPred scst p
             Not p     -> not $ evalPred scst p
             And p1 p2 -> evalPred scst p1 && evalPred scst p2
             Or p1 p2  -> evalPred scst p1 || evalPred scst p2
             Pred c    -> evalComp scst c
-- Calculates the result of a top-level command execution

findCommand commandToFind t = fromMaybe (error ("command " ++ commandToFind ++ " does not exist\n"))$(M.lookup commandToFind (t))

runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel allCommands scst (TLCmd  comd) = do 
										let getVar = unWrp (vartable scst)
										case comd of
											(Cmd _ _ _ _ _) -> runCmd comd allCommands scst
											(Assign var val) -> do
											  let newVarTable = M.insert (getVar var) (getVar val) (vartable scst);
											  return scst{vartable = newVarTable}
										

										
runTopLevel allCommands scst (TLCnd  cond) = do
											case cond of
												(If condition cmds1) ->do
															let cmds = (if evalPred scst condition then cmds1 else [])
															newScst <- runHashProgram allCommands (Right scst) (map TLCmd cmds)
															return newScst
												
												(IfElse condition cmds1 cmds2) -> do
															let cmds = (if evalPred scst condition then cmds1 else cmds2)
															newScst <- runHashProgram allCommands (Right scst) (map TLCmd cmds)
															return newScst
															
															
runTopLevel allCommands scst (TLwl loop) = do
											case loop of 
												(While condition cmds) -> do
															if (evalPred scst condition) then
															 (do 
																newScst <- runHashProgram allCommands (Right scst) (map TLCmd cmds)
																runTopLevel allCommands newScst (TLwl loop)
																)
															else
																return scst