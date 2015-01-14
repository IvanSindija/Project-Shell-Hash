module Parsing.HashParser where

import Language.Expressions
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, spaces, letter, satisfy, alphaNum, anyChar, noneOf, oneOf)
import Text.Parsec (parse, ParseError, try, optionMaybe)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.Expr
import Control.Applicative ((<|>), (<$>), (<$), (<*>), (<*), (*>), Applicative, many)
import Control.Monad (when)
import Data.Char (digitToInt, isAlphaNum, isLetter)
import Data.Maybe (isNothing, fromMaybe)

err = "An error has occurred"

token :: Parser a -> Parser a
token = (<* spaces)

parseStringToTLExpr :: String -> [TLExpr]
parseStringToTLExpr = parsed . betterParse (many tlexpr)

parsed (Right s) = s
parsed (Left _)  = error err

betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

parsString :: Parser String
parsString = token $ many1 (noneOf ['=', ' ', '<', '>', '(', ')', '\n', ';', '{', '}'])

litString :: Parser String
litString = token $ many (noneOf ['"'])

stringLiteral :: Parser Expr
stringLiteral = do
  token $ char '"'
  ret <- Str <$> litString
  token $ char '"'
  return ret

strExpr :: Parser Expr
strExpr = Str <$> parsString

varExpr :: Parser Expr
varExpr = Var <$> variable

variable :: Parser String
variable = do
  char '$'
  first <- satisfy (\c -> isLetter c )
  rest <- many (satisfy (\c -> isAlphaNum c ))
  spaces
  return (first:rest)


expr :: Parser Expr
expr = try varExpr <|> strExpr

manyExpr = many expr

symbol :: Char -> Parser Char
symbol = token . char

assign :: Parser Cmd
assign = do
  varName <- strExpr
  char '='
  value <- strExpr
  spaces
  symbol ';'
  return (Assign varName value)
  

inputRedir :: Parser Expr
inputRedir = do
  char '<'
  spaces
  redirExpr <- expr
  return redirExpr
  
outputRedir :: Parser (Expr, Bool)
outputRedir = do
  char '>'
  appended <- optionMaybe (char '>')
  spaces
  redirExpr <- expr
  spaces
  return (redirExpr, not $ isNothing appended)
 

cmd :: Parser Cmd
cmd = do
  cmdName <- expr
  spaces
  cmdArgs <- many (try stringLiteral <|> expr)
  spaces
  redirIn <- optionMaybe inputRedir
  redirOut <- optionMaybe outputRedir
  spaces
  let (outputExpr, isAppended) = case redirOut of
                                   Nothing          -> (Nothing, False)
                                   Just (ex, isApp) -> (Just ex, isApp)
  symbol ';'                                 
  return (Cmd cmdName cmdArgs redirIn outputExpr isAppended)
  
  
cmdOrAssign :: Parser Cmd
cmdOrAssign = try assign <|>  cmd
    
comp :: Parser Comp
comp = do
  expr1 <- expr
  spaces
  op <- many1 (oneOf ['=', '/', '>', '<'])
  spaces
  expr2 <- expr
  spaces
  let opConst = case op of
                    "==" -> CEQ
                    "/=" -> CNE
                    ">=" -> CGE
                    ">"  -> CGT
                    "<=" -> CLE
                    "<"  -> CLT
  return (opConst expr1 expr2)


table = [[unary '!' Not], [binary '&' And], [binary '|' Or]]
  where binary sym f = Infix (mkParser sym f) AssocLeft
        mkParser s f = do
          char s
          spaces
          return f
        unary sym f = Prefix (mkParser sym f)

prd :: Parser Pred
prd = buildExpressionParser table other
  where other = cmp <|> parenPred
        cmp = do
          c <- comp
          spaces
          return (Pred c)
        parenPred = do
          char '('
          pr <- prd
          char ')'
          spaces
          return (Parens pr)

clause :: Parser [Cmd]
clause = do
  symbol '{'
  cmds <- many cmdOrAssign
  symbol '}'
  return (cmds)
  

ifOrIfElse :: Parser Conditional
ifOrIfElse = try ifElse <|> if'
  
ifElse :: Parser Conditional
ifElse = do
  string "if"
  spaces
  char '('
  cnd <- prd
  spaces
  char ')'
  spaces
  cmds1 <- clause
  spaces
  el <- string "else"
  spaces
  cmds2 <- clause
  spaces
  fi <- string "end"
  spaces
  return (IfElse cnd cmds1 cmds2)

if' :: Parser Conditional
if' = do
  string "if"
  spaces
  char '('
  cnd <- prd
  spaces
  char ')'
  spaces
  cmds1 <- clause
  fi <- string "end"
  spaces
  return (If cnd cmds1 )

while :: Parser Loop
while = do
  string "while"
  spaces
  char '('
  cnd <- prd
  spaces
  char ')'
  spaces
  cmds1 <- clause
  fi <- string "end"
  spaces
  return (While cnd cmds1 )
  
tlexpr :: Parser TLExpr
tlexpr = (try (TLCmd <$> cmdOrAssign)) <|> (try (TLCnd <$> ifOrIfElse)) <|> (TLwl <$> while)