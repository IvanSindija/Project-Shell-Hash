module Language.Expressions where
-- Contains the data structures describing the structure of the language itself
-- You are free to use a different structure, as long as it describes a similar
-- enough language sufficiently well.
-- A command which performs something - can be a command that takes arguments
-- or an assignment.
data Cmd = Cmd { name :: Expr -- The command name (can be a variable)
, args :: [Expr] -- The command arguments
, inDir :: Maybe Expr -- A redirected input fp
, outDir :: Maybe Expr -- A redirected output fp
, append :: Bool -- If redirected, is it appending?
} | Assign { var :: Expr -- Assignment target
, val :: Expr -- A value to assign to a variable
} deriving Show
-----------------------------------------------------------------------------------------------------
-- A bottom-level expression
data Expr = Var String -- A named variable
 | Str String deriving (Eq, Show) -- A mere string, the peasant of expressions
----------------------------------------------------------------------------------------------------
-- A comparison operation
data Comp = CEQ Expr Expr -- ==
 | CNE Expr Expr -- /=
 | CGE Expr Expr -- >=
 | CGT Expr Expr -- >
 | CLE Expr Expr -- <=
 | CLT Expr Expr -- <
 | CLI Expr deriving (Eq, Show) -- A wrapped expression literal - True if nonempty
-----------------------------------------------------------------------------------------------------
-- Something that evaluates to a truth value
data Pred = Pred Comp -- A wrapped comparison
 | Not Pred -- Negation
 | And Pred Pred -- A binary logical and
 | Or Pred Pred -- A binary logical or
 | Parens Pred deriving (Eq, Show)-- An expression in parentheses
------------------------------------------------------------------------------------------------------
-- A conditional branching expression - if-then or if-then-else
-- If-then with a condition and a list of actions
data Conditional = If { cond :: Pred -- Predicate to satisfy
, cthen :: [Cmd] -- Actions if satisfied
}
-- An if-then-else with a condition and two possible paths
 | IfElse { cond :: Pred -- Predicate to satisfy
, cthen :: [Cmd] -- Actions if satisfied
, celse :: [Cmd] -- Actions otherwise
} deriving Show
--------------------------------------------------------------------------------------------------------
data Loop = While { cond' :: Pred
, cthen' :: [Cmd]
} deriving Show
--------------------------------------------------------------------------------------------------------
-- A top-level expression, wrapping either a conditional expression or a
-- command
data TLExpr = TLCmd Cmd | TLCnd Conditional | TLwl Loop deriving Show