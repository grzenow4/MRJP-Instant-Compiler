module LLVMCompiler where

import Control.Monad.State
import qualified Data.Set as Set

import Instant.AbsInstant

type Var = String
type Reg = Int
type Env = Set.Set Var
data St = St {
    env :: Env,
    reg :: Reg
}

type CM a = StateT St IO a

data Ret = Reg Reg | Val Integer
data Instr = Print Ret | Alloc Var | Store Ret Var | Load Ret Var
           | Add Ret Ret Ret | Sub Ret Ret Ret | Mul Ret Ret Ret | Div Ret Ret Ret

instance Show Ret where
    show v = case v of
        Reg r -> "%" ++ show r
        Val i -> show i

initState :: St
initState = St { env = Set.empty, reg = 0 }

takeStr :: Ident -> String
takeStr (Ident x) = x

newReg :: CM Ret
newReg = do
    modify (\s -> s { reg = (reg s) + 1 })
    Reg <$> gets reg

getVar :: Ret -> Var -> CM String
getVar reg x = do
    env <- gets env
    if Set.member x env
    then llvmInstr (Load reg x)
    else error $ "Variable " ++ x ++ " does not exist."

writeVar :: Var -> CM String
writeVar x = do
    env_ <- gets env
    if Set.member x env_
    then return ""
    else modify (\s -> s { env = Set.insert x (env s) }) >> llvmInstr (Alloc x)

returnFmt :: String -> CM String
returnFmt s = return $ "    " ++ s ++ "\n"

llvmInstr :: Instr -> CM String
llvmInstr (Print v)       = returnFmt $ "call void @printInt(i32 " ++ show v ++ ")"
llvmInstr (Alloc x)       = returnFmt $ "%" ++ x ++ " = alloca i32"
llvmInstr (Store reg x)   = returnFmt $ "store i32 " ++ show reg ++ ", i32* %" ++ x
llvmInstr (Load reg x)    = returnFmt $ show reg ++ " = load i32, i32* %" ++ x
llvmInstr (Add reg v1 v2) = returnFmt $ show reg ++ " = add i32 " ++ show v1 ++ ", " ++ show v2
llvmInstr (Sub reg v1 v2) = returnFmt $ show reg ++ " = sub i32 " ++ show v1 ++ ", " ++ show v2
llvmInstr (Mul reg v1 v2) = returnFmt $ show reg ++ " = mul i32 " ++ show v1 ++ ", " ++ show v2
llvmInstr (Div reg v1 v2) = returnFmt $ show reg ++ " = sdiv i32 " ++ show v1 ++ ", " ++ show v2

compilePrg :: [Stmt] -> CM String
compilePrg ss = do
    program <- mapM compileStmt ss
    return $ concat program

compileStmt :: Stmt -> CM String
compileStmt (SAss ident e) = do
    (v, code1) <- compileExp e
    let x = takeStr ident
    code2 <- writeVar x
    code3 <- llvmInstr (Store v x)
    return $ code1 ++ code2 ++ code3
compileStmt (SExp e) = do
    (v, code1) <- compileExp e
    code2 <- llvmInstr (Print v)
    return $ code1 ++ code2

compileExp :: Exp -> CM (Ret, String)
compileExp (ExpLit n) = return (Val n, "")
compileExp (ExpVar x) = do
    reg <- newReg
    code <- getVar reg (takeStr x)
    return (reg, code)
compileExp (ExpAdd e1 e2) = do
    (v1, code1) <- compileExp e1
    (v2, code2) <- compileExp e2
    reg <- newReg
    code3 <- llvmInstr (Add reg v1 v2)
    return (reg, code1 ++ code2 ++ code3)
compileExp (ExpSub e1 e2) = do
    (v1, code1) <- compileExp e1
    (v2, code2) <- compileExp e2
    reg <- newReg
    code3 <- llvmInstr (Sub reg v1 v2)
    return (reg, code1 ++ code2 ++ code3)
compileExp (ExpMul e1 e2) = do
    (v1, code1) <- compileExp e1
    (v2, code2) <- compileExp e2
    reg <- newReg
    code3 <- llvmInstr (Mul reg v1 v2)
    return (reg, code1 ++ code2 ++ code3)
compileExp (ExpDiv e1 e2) = do
    (v1, code1) <- compileExp e1
    (v2, code2) <- compileExp e2
    reg <- newReg
    code3 <- llvmInstr (Div reg v1 v2)
    return (reg, code1 ++ code2 ++ code3)

compile :: Program -> String -> IO String
compile (Prog stmts) _ = do
    (code, _) <- runStateT (compilePrg stmts) initState
    return $ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\n" ++
             "declare i32 @printf(i8*, ...)\n\n" ++
             "define void @printInt(i32 %x) {\n" ++
             "    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
             "    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n" ++
             "    ret void\n" ++
             "}\n\n" ++
             "define i32 @main() {\n" ++
             code ++
             "    ret i32 0\n" ++
             "}\n"
