module Logic where

import Data.List (nub)
import Control.Monad (replicateM)
import Prelude hiding (lookup)

-- propositional logic datatype
data PropL = PVar String | PNot PropL | PropL `PAnd` PropL | PropL `POr` PropL deriving Eq

-- custom show instance to pretty print formulas
instance Show PropL where
    show (PVar s) = s
    show (PNot p) = "~" ++ show p
    show (p `PAnd` q) = "(" ++ show p ++ " & " ++ show q ++ ")"
    show (p `POr` q) = "(" ++ show p ++ " | " ++ show q ++ ")"

-- counts the number of operators in a sentence
opsNr :: PropL -> Int
opsNr (PVar _) = 0
opsNr (PNot p) = 1 + opsNr p
opsNr (PAnd p q) = 1 + opsNr p + opsNr q
opsNr (POr p q) = 1 + opsNr p + opsNr q

-- computes the depth of a formula
depth :: PropL -> Int
depth (PVar _) = 0
depth (PNot p) = 1 + depth p
depth (PAnd p q) = 1 + max (depth p) (depth q)
depth (POr p q) = 1 + max (depth p) (depth q)

-- creates a list of variables in a formula
gatherNames :: PropL -> [String]                  
gatherNames (PVar s) = [s]                        
gatherNames (PNot p) = gatherNames p                
gatherNames (PAnd p q) = gatherNames p ++ gatherNames q
gatherNames (POr p q) = gatherNames p ++ gatherNames q

-- as above but remove duplicates
propNames :: PropL -> [String]
propNames = nub . gatherNames

-- recursive de Morgan's
dM :: PropL -> PropL
dM (PNot (p `PAnd` q)) = PNot (dM p) `POr` PNot (dM q)
dM (PNot (p `POr` q)) = PNot (dM p) `PAnd` PNot (dM q)
dM (PNot p) = PNot (dM p)
dM (p `PAnd` q) = dM p `PAnd` dM q
dM (p `POr` q) = dM p `POr` dM q
dM (PVar p) = PVar p

-- recursive DNE
dne :: PropL -> PropL
dne (PNot (PNot p)) = dne p
dne (PNot p) = PNot (dne p)
dne (p `PAnd` q) = dne p `PAnd` dne q 
dne (p `POr` q) = dne p `POr` dne q 
dne (PVar p) = PVar p

-- recursive distributive law
distLaw :: PropL -> PropL 
distLaw ((p `PAnd` q) `POr` (r `PAnd` s)) = (distLaw p `POr` distLaw r) `PAnd` (distLaw p `POr` distLaw s) `PAnd` (distLaw q `POr` distLaw r) `PAnd` (distLaw q `POr` distLaw s) -- double distributivity
distLaw (p `POr` (q `PAnd` r)) = (distLaw p `POr` distLaw q) `PAnd` (distLaw p `POr` r) --left dist
distLaw ((q `PAnd` r) `POr` p) = (distLaw q `POr` distLaw p) `PAnd` (distLaw r `POr` distLaw p) --right dist
distLaw (PNot p) = PNot (distLaw p)
distLaw (p `PAnd` q) = distLaw p `PAnd` distLaw q
distLaw (p `POr` q) = distLaw p `POr` distLaw q
distLaw (PVar p) = PVar p

-- to conjunctive normal form
toCNF = distLaw . dne . dM

-- data types for assignments and truth tables
type VarAssignment = [(String,Bool)]
type TruthTable = [(VarAssignment,Bool)]

-- generating assignments
mkAssignments :: [String] -> [VarAssignment]
mkAssignments vs = [ zip vs ts | ts <- replicateM (length vs) [True,False]]

-- unsafe lookup
lookup :: (Eq a) => [(a,b)] -> a -> b
lookup [] _ = undefined
lookup ((key,val):ps) x = if key == x then val else lookup ps x

-- denotation function
interpretAtA :: VarAssignment -> PropL -> Bool
interpretAtA a (PVar v) = lookup a v
interpretAtA a (PNot p) = not (interpretAtA a p)
interpretAtA a (p `PAnd` q) = interpretAtA a p && interpretAtA a q
interpretAtA a (p `POr` q) = interpretAtA a p || interpretAtA a q

-- truth table generator
toTruthTable :: PropL -> [(VarAssignment,Bool)]
toTruthTable p = [(a, interpretAtA a p) | a <- mkAssignments (propNames p)]

-- pretty printing a truth table
printBool :: Bool -> String
printBool True = "1"
printBool False = "0"

prettyPrintAssignment :: VarAssignment -> String
prettyPrintAssignment [] = ""
prettyPrintAssignment ((v,t):as) = printBool t ++ " " ++ prettyPrintAssignment as


prettyPrintTable :: [(VarAssignment,Bool)] -> String
prettyPrintTable [] = ""
prettyPrintTable ((a,t):ls) = prettyPrintAssignment a ++ "| " ++ printBool t ++ "\n" ++ prettyPrintTable ls
