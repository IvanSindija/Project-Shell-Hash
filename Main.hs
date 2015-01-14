module Main where
import Data.List
import Hash
import System.Environment
main = do
	args <- getArgs
	if(2 == ( length args) ) then runScript $ last args else runInteractive