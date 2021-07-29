module StackInterpreter where

import qualified Data.Map as Map
import Data.Char

data Instruction = 
    PUSH                { value :: StackValue }                     |
    POP                                                             |
    INTBINOP            { operator1 :: (Int -> Int -> Int) }        |
    BOOLBINOP           { operator2 :: (Bool -> Bool -> Bool) }     |
    COMPOP              { operator3 :: (Int -> Int -> Bool) }       |
    EQ_                                                             |
    NEQ                                                             |
    UMINUS                                                          |
    NOT                                                             |
    RVAL                { varId :: String }                         |
    LVAL                { varId :: String }                         |
    ASSIGN                                                          |
    GOTO                { label :: String }                         |
    CONDGO              { label :: String, cond :: (Bool -> Bool) } |
    READ                { varId :: String }                         |
    PRINT               { varId :: String }                         |
    EXIT                                                            |
    RESET                                                           |
    ERROR               { message :: String }                       



type Program = [Instruction]

data StackValue
    = INT  { getInt :: Int }
    | BOOL { getBool :: Bool }
    | LVALUE { getLval :: String }

type Stack = [StackValue]

type VarValues = Map.Map String StackValue

type Labels = Map.Map String Int

-- ------------------------------------------------

newStack :: Stack
newStack = []

newVarValues :: VarValues
newVarValues = Map.empty

goto :: Int -> Program -> Program
goto = drop

push :: StackValue -> Stack -> Stack
push s stk = s:stk

pop :: Stack -> Maybe Stack
pop [] = Nothing
pop (s:ss) = Just ss

topPop :: Stack -> IO (Maybe (StackValue, Stack))
topPop [] = return Nothing
topPop (s:ss) = return $ Just (s,ss)

addVal :: VarValues -> String -> StackValue -> VarValues
addVal varToVal lv sv = Map.insert lv sv varToVal

getVal :: String -> VarValues -> Maybe StackValue
getVal varid varToVal = Map.lookup varid varToVal 


isNum :: String -> Bool
isNum ('-':n) = all isDigit n
isNum n = all isDigit n

-- ------------------------------------------------

runProgram :: Program -> Labels -> IO ()
runProgram instructions labels =
    runProgram' instructions instructions labels newVarValues newStack


runProgram' :: Program -> Program -> Labels -> VarValues -> Stack -> IO ()
runProgram' _ [] _ _ _ = pure ()
runProgram' allInst (currIntr:instr) labels varToVal stack = do
    runIntruction currIntr allInst instr labels varToVal stack


runIntruction :: Instruction -> Program -> Program -> Labels -> VarValues -> Stack -> IO ()

runIntruction (PUSH val) allInst instr labels varToVal stack = do
    runProgram' allInst instr labels varToVal (push val stack)

runIntruction (POP) allInst instr labels varToVal stack = do
    let maybeStack = pop stack
    case maybeStack of
        Just stack' -> runProgram' allInst instr labels varToVal stack'
        Nothing -> do
            putStrLn "[Error] In Pop Instruction. Empty Stack."
            runProgram' allInst instr labels varToVal stack

runIntruction (INTBINOP op) allInst instr labels varToVal stack =
    if length stack < 2 
        then do
            putStrLn "[Error] In aritmethic operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (r, stack')  <- topPop stack
            Just (l, stack'') <- topPop stack'
            case (l, r) of
                (INT lv, INT rv) -> 
                    runProgram' allInst instr labels varToVal (push (INT $ op lv rv) stack'')
                _ -> do
                    putStrLn "[Error] In aritmethic operation. Non Integer arguments."
                    runProgram' allInst instr labels varToVal stack

runIntruction (BOOLBINOP op) allInst instr labels varToVal stack =
    if length stack < 2 
        then do
            putStrLn "[Error] In boolean operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (r, stack')  <- topPop stack
            Just (l, stack'') <- topPop stack'
            case (l, r) of
                (BOOL lv, BOOL rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ op lv rv) stack'')
                _ -> do
                    putStrLn "[Error] In boolean operation. Non boolean arguments."
                    runProgram' allInst instr labels varToVal stack

runIntruction (COMPOP op) allInst instr labels varToVal stack =
    if length stack < 2 
        then do
            putStrLn "[Error] In comparation operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (r, stack')  <- topPop stack
            Just (l, stack'') <- topPop stack'
            case (l, r) of
                (INT lv, INT rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ op lv rv) stack'')
                _ -> do
                    putStrLn "[Error] In comparation operation. Non Integer arguments."
                    runProgram' allInst instr labels varToVal stack

runIntruction (EQ_) allInst instr labels varToVal stack =
    if length stack < 2 
        then do
            putStrLn "[Error] In Equal operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (r, stack')  <- topPop stack
            Just (l, stack'') <- topPop stack'
            case (l, r) of
                (INT lv, INT rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ lv == rv) stack'')
                (BOOL lv, BOOL rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ lv == rv) stack'')
                (LVALUE lv, LVALUE rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ lv == rv) stack'')
                _ -> do
                    putStrLn "[Error] In Equal operation. Arguments of diferent types."
                    runProgram' allInst instr labels varToVal stack

runIntruction (NEQ) allInst instr labels varToVal stack =
    if length stack < 2 
        then do
            putStrLn "[Error] In Equal operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (r, stack')  <- topPop stack
            Just (l, stack'') <- topPop stack'
            case (l, r) of
                (INT lv, INT rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ lv /= rv) stack'')
                (BOOL lv, BOOL rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ lv /= rv) stack'')
                (LVALUE lv, LVALUE rv) -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ lv /= rv) stack'')
                _ -> do
                    putStrLn "[Error] In Equal operation. Arguments of diferent types."
                    runProgram' allInst instr labels varToVal stack

runIntruction (UMINUS) allInst instr labels varToVal stack =
    if length stack < 1 
        then do
            putStrLn "[Error] In Unary Minus operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (v, stack')  <- topPop stack
            case v of
                (INT v') -> 
                    runProgram' allInst instr labels varToVal (push (INT $ -v') stack')
                _ -> do
                    putStrLn "[Error] In Unary Minus operation. Non Integer argument."
                    runProgram' allInst instr labels varToVal stack

runIntruction (NOT) allInst instr labels varToVal stack =
    if length stack < 1 
        then do
            putStrLn "[Error] In Boolean Negation operation. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (v, stack')  <- topPop stack
            case v of
                (BOOL v') -> 
                    runProgram' allInst instr labels varToVal (push (BOOL $ not v') stack')
                _ -> do
                    putStrLn "[Error] In Boolean Negation operation. Non Boolean argument."
                    runProgram' allInst instr labels varToVal stack

runIntruction (RVAL varid) allInst instr labels varToVal stack = do
    let val = getVal varid varToVal
    case val of
        Just v  -> runProgram' allInst instr labels varToVal (push v stack)
        Nothing -> do
            putStrLn $ "[Error] In RValue operation. Non value assigned to " ++ varid ++ "."
            runProgram' allInst instr labels varToVal stack

runIntruction (LVAL varid) allInst instr labels varToVal stack =
    runProgram' allInst instr labels varToVal (push (LVALUE varid) stack)

runIntruction (ASSIGN) allInst instr labels varToVal stack =
    if length stack < 2 
        then do
            putStrLn "[Error] In assign instruction. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal stack
        else do
            Just (lval, stack')  <- topPop stack
            Just (val, stack'')  <- topPop stack'
            case (lval, val) of
                (LVALUE lv, INT rv) -> do
                    let varToVal' = addVal varToVal lv (INT rv)
                    runProgram' allInst instr labels varToVal' stack''
                (LVALUE lv, BOOL rv) -> do
                    let varToVal' = addVal varToVal lv (BOOL rv)
                    runProgram' allInst instr labels varToVal' stack''
                _ -> do
                    putStrLn "[Error] In assign instruction. Non valid arguments."
                    runProgram' allInst instr labels varToVal stack

runIntruction (GOTO label) allInst instr labels varToVal stack = do
    let position = Map.lookup label labels
    case position of
        Just pos -> runProgram' allInst (goto pos allInst) labels varToVal stack
        Nothing  -> do
            putStrLn "[Error] In Goto instruction. Non valid label."
            runProgram' allInst instr labels varToVal stack

runIntruction (CONDGO label cond) allInst instr labels varToVal (stk:stack) = 
    if length stack < 1 
        then do
            putStrLn "[Error] In Goto conditional instruction. Not enough arguments in stack."
            runProgram' allInst instr labels varToVal (stk:stack)
        else do
            let position = Map.lookup label labels
            case (position, stk) of
                (Just pos, BOOL v) -> if cond v 
                    then runProgram' allInst (goto pos allInst) labels varToVal stack
                    else runProgram' allInst instr labels varToVal stack
                (Just pos, _) -> do
                    putStrLn "[Error] In Goto conditional instruction. Non Boolean argument."
                    runProgram' allInst instr labels varToVal (stk:stack)
                (Nothing, _)  -> do
                    putStrLn "[Error] In Goto conditional instruction. Non valid label."
                    runProgram' allInst instr labels varToVal (stk:stack)

runIntruction (READ varid) allInst instr labels varToVal stack = do
    s <- getLine
    case s of
        "true" -> do
            let varToVal' = addVal varToVal varid (BOOL True)
            runProgram' allInst instr labels varToVal' stack
        "false"-> do
            let varToVal' = addVal varToVal varid (BOOL False)
            runProgram' allInst instr labels varToVal' stack
        n -> if isNum n
            then do
                let varToVal' = addVal varToVal varid (INT $ read n)
                runProgram' allInst instr labels varToVal' stack
            else do
                putStrLn "[Error] In Read instruction. Non valid input."
                runProgram' allInst instr labels varToVal stack
                

runIntruction (PRINT varid) allInst instr labels varToVal stack = 
    case Map.lookup varid varToVal of
        Just sv -> do
            case sv of
                INT n -> print n
                BOOL b -> print b
                LVALUE l -> print l
            runProgram' allInst instr labels varToVal stack
        _ -> do
            putStrLn $ "[Error] In Print instruction. No value associated with the identifier " ++varid++"."
            runProgram' allInst instr labels varToVal stack

runIntruction (EXIT) allInst instr labels varToVal stack = pure ()

runIntruction (RESET) allInst instr labels varToVal stack =
    runProgram' allInst instr labels newVarValues newStack

runIntruction (ERROR mssg) allInst instr labels varToVal stack = do
    putStrLn mssg
    runProgram' allInst instr labels varToVal stack
    
