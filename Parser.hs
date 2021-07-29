module Parser where

import qualified Data.Map as Map
import qualified StackInterpreter as SI
import Data.Char 

parse :: [[String]] -> ([SI.Instruction], SI.Labels)
parse tks = parse' 0 tks


parse' :: Int -> [[String]] -> ([SI.Instruction], SI.Labels)
parse' _ [] = ([], Map.empty)
parse' i ([]:tks) = parse' i tks
parse' i (tkline:tks) = (inst:insts, labels')
    where
        (inst, label) = parseLine tkline
        (insts, labels) = parse' (i+1) tks
        labels' = case label of 
            Just l -> Map.insert l i labels
            _      -> labels


parseLine :: [String] -> (SI.Instruction, Maybe String)
parseLine [] = error "This should never happen."
parseLine l@(tk:tks) = if (last tk == ':')
    then (lineToInst tks, Just $ init tk)
    else (lineToInst l, Nothing)


lineToInst :: [String] -> SI.Instruction
lineToInst ["PUSH","true"]      = SI.PUSH (SI.BOOL True)
lineToInst ["PUSH","false"]     = SI.PUSH (SI.BOOL False)
lineToInst ["PUSH",value]    
    | SI.isNum value            = SI.PUSH (SI.INT (read value ::Int))
    | otherwise                 = SI.ERROR "[Error]: Push: Non valid input in push instruction."
lineToInst ["POP"]              = SI.POP 
lineToInst ["ADD"]              = SI.INTBINOP (+) 
lineToInst ["SUB"]              = SI.INTBINOP (-) 
lineToInst ["MUL"]              = SI.INTBINOP (*) 
lineToInst ["DIV"]              = SI.INTBINOP (div) 
lineToInst ["AND"]              = SI.BOOLBINOP (&&)
lineToInst ["OR"]               = SI.BOOLBINOP (||)
lineToInst ["LT"]               = SI.COMPOP (<)
lineToInst ["LE"]               = SI.COMPOP (<=)
lineToInst ["GT"]               = SI.COMPOP (>)
lineToInst ["GE"]               = SI.COMPOP (>=)
lineToInst ["EQ"]               = SI.EQ_ 
lineToInst ["NEQ"]              = SI.NEQ
lineToInst ["UMINUS"]           = SI.UMINUS
lineToInst ["NOT"]              = SI.NOT
lineToInst ["RVALUE",var]       = SI.RVAL var
lineToInst ["LVALUE",var]       = SI.LVAL var
lineToInst ["ASSIGN"]           = SI.ASSIGN
lineToInst ["GOTO",label]       = SI.GOTO label
lineToInst ["GOTRUE",label]     = SI.CONDGO label (== True)
lineToInst ["GOFALSE",label]    = SI.CONDGO label (== False)
lineToInst ["READ",var]         = SI.READ var
lineToInst ["PRINT",var]        = SI.PRINT var
lineToInst ["EXIT"]             = SI.EXIT
lineToInst ["RESET"]            = SI.RESET
lineToInst err                  = SI.ERROR $ "[Parsing Error]: Not a valid instruction. At line: " ++ (unwords err)