module Language.Commands where
import Language.Exec
import Language.Expressions
import qualified Data.ByteString.Char8 as Bc
import qualified Data.ByteString       as B  (length, unpack)
import Data.ByteString                       (ByteString)
import System.Directory
import Numeric                               (showHex)
import Data.Char                             (isAscii, isControl)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad (void, mapM,when,forM_)
import Data.List
import System.FilePath

allCommands =  M.fromList ([("echo",echo),("inc",inc),("ls",ls),("pwd",pwd),("cat",cat),("cd",cd),("mkDir",mkDir),("rmDir",rmDir),
	("cpdir",cpdir),("mvFile",mvFile),("mvFiles",mvFiles),("cpFile",cpFile),("cpFiles",cpFiles),("rm",rm),("crFiles",crFiles),("hexDump",hexDump)])

echo :: Command
echo args scst = return (scst {output =(getVarFromString (vartable scst) (head args) []) ++ "\n"})

getVarFromString :: VarTable -> String -> String -> String
getVarFromString vt [s,v] acc = if s=='$' then (acc++(getVar [v] vt)) else (acc++[s,v])
getVarFromString vt (s:v:rs) acc = if s=='$' then (getVarFromString vt rs (acc++(getVar [v] vt))) else (getVarFromString vt (v:rs) (acc++[s]))


--concatMap (\(f,s) -> if odd s then (getVar f vt) else f) $ findVars str 0
  --where findVars "" i = [("",i)]
    --    findVars (c:str) i
      --    | c == '$'  = ("", i) : findVars str (i+1)
        --  | otherwise = let ((h,ind):t) = findVars str i in (((c:h),ind):t)
 
getVar:: String->VarTable->String
getVar str vt = fromMaybe (error "variable " ++ str ++  " does not exist\n") $ M.lookup str vt
		
cat :: Command
cat args scst@(ScriptState _ wd _) = do
    let fn = head args
    x <- readFile $ combine wd fn
    return ( scst { output = x} )
	
pwd :: Command
pwd args scst@(ScriptState _ wd _) = return (scst {output = wd})

ls:: Command
ls [] scst@( ScriptState _ wd _)= do
									subDirs<- getDirectoryContents wd
									let cont = filter (`notElem` [".", ".."]) subDirs 
									return (scst {output = unlines cont})
ls args scst = do
				  subDirs <- getDirectoryContents $ head args 
				  let cont = filter (`notElem` [".", ".."]) subDirs
				  return (scst {output = (unlines cont)})
				  
cd :: Command
cd [] scst = do
				hd <-getHomeDirectory
				return (scst {output = hd ,wd = hd})
cd args scst@(ScriptState _ wrd _) | ( head args) == ".." = return (scst {output = takeDirectory wrd , wd = takeDirectory wrd}) 
								   | otherwise = do 
													let dir = combine wrd $ head args
													tf <- doesDirectoryExist dir
													if(tf)	then return ( scst {output = dir , wd = dir}) else return ( scst {output = "invalid directory"})
								   


mkDir :: Command
mkDir args scst@(ScriptState _ wd _) = do 
					createDirectory ( combine wd $ head args)
					return (scst {output = "Directory created"})  
					
rmDir :: Command
rmDir args scst@(ScriptState _ wd _ ) = do 
					removeDirectory ( combine wd $ head args)
					return (scst {output = "Directory removed"})


copy args wd = do
		let src = combine wd $ head args
		let des = combine wd ( combine ( last args) ( head args) )
		createDirectory des
		dirs' <- getDirectoryContents src
		let cout = ( filter (`notElem` [".", ".."]) dirs' )
		forM_ cout ( \name -> do 
			let srcPath = combine src name
			let destPath = combine des name
			isDir <- doesDirectoryExist srcPath
			if isDir then copy [srcPath,destPath] wd else copyFile srcPath destPath )
		
cpdir :: Command
cpdir args scst@(ScriptState _ wd _ ) = do
								copy args wd
								return (scst {output = "Dir copied"})

        
mvFile :: Command
mvFile [src,dest] scst@(ScriptState _ wd _ ) = do 
														cpFile [src,dest] scst 
														removeFile (combine wd src)
														return (scst {output = "File is moved"})
														
mvFiles :: Command
mvFiles [des] scst = return (scst {output = "files moved"})
mvFiles (x:src) scst = do  
							mvFile [x,last src] scst
							mvFiles src scst
							

cpFile [src,dest]  scst@(ScriptState _ wd _) = do
												copyFile (combine wd src) (combine (combine wd dest) $  takeFileName src)
												return (scst {output = "File copied"})

cpFiles :: Command
cpFiles [dir] scst = return (scst{ output = "Files copied"})												
cpFiles (f:fs) scst = do 
										cpFile [f,last fs] scst
										cpFiles fs scst

rm :: Command
rm []  scst= return (scst {output = "Files removed"})
rm (f:files) scst@(ScriptState _ wd _) = do 
											removeFile (combine wd f)
											rm files scst

crFiles :: Command											
crFiles [] scst = return (scst {output = "Files created"})
crFiles (f:files) scst@(ScriptState _ wd _) = do
													writeFile (combine wd f) ""
													crFiles files scst
													
inc :: Command
inc args scst@(ScriptState _ _ vr) = do
										let na = show ((read (getVar  (head args) vr) :: Double)+1)
										return scst{output="",vartable = (M.insert (head args) na vr)}
										
hexDump :: Command
hexDump args scst@(ScriptState _ wd _) =do
					bs<- Bc.readFile (combine wd $ head args)
					return scst {output = simpleHex bs}
					
					
					
byteWidth    = 2

paddedShowHex :: (Show a, Integral a) => Int -> a -> String
paddedShowHex w n = pad ++ str
    where
     str = showHex n ""
     pad = replicate (w - length str) '0'

simpleHex :: ByteString -> String
simpleHex = intercalate ","
          . map (paddedShowHex 2)
          . B.unpack
