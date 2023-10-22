module JVMCompiler where

import Control.Monad.State
import qualified Data.Map as Map
import System.FilePath.Posix (takeBaseName)

import Instant.AbsInstant

type Var = String
type Loc = Int
type Env = Map.Map Var Loc
type CM a = StateT Env IO a

data Instr = PrintStream
           | IAdd | ISub | IMul | IDiv
           | IConstM1 | IConst Integer | BiPush Integer | SiPush Integer | Ldc Integer
           | IStore Loc | ILoad Loc

initEnv :: Env
initEnv = Map.empty

takeStr :: Ident -> String
takeStr (Ident x) = x

computeStackSize :: Int -> Int -> Int
computeStackSize s1 s2 = if s1 == s2 then s1 + 1 else max s1 s2

newLoc :: CM Loc
newLoc = Map.size <$> get

writeLoc :: Var -> CM Loc
writeLoc x = do
    env <- get
    case Map.lookup x env of
        Just l -> return l
        Nothing -> do
            l <- newLoc
            modify $ Map.insert x l
            return l

getLoc :: Var -> CM Loc
getLoc x = do
    env <- get
    case Map.lookup x env of
        Just l -> return l
        Nothing -> error $ "Variable " ++ x ++ " does not exist."

returnFmt :: String -> CM String
returnFmt s = return $ "    " ++ s ++ "\n"

jvmInstr :: Instr -> CM String
jvmInstr PrintStream = returnFmt $ "getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
                                   "    swap\n" ++
                                   "    invokevirtual java/io/PrintStream/println(I)V"
jvmInstr IAdd        = returnFmt   "iadd"
jvmInstr ISub        = returnFmt   "isub"
jvmInstr IMul        = returnFmt   "imul"
jvmInstr IDiv        = returnFmt   "idiv"
jvmInstr IConstM1    = returnFmt   "iconst_m1"
jvmInstr (IConst n)  = returnFmt $ "iconst_" ++ show n
jvmInstr (BiPush n)  = returnFmt $ "bipush " ++ show n
jvmInstr (SiPush n)  = returnFmt $ "sipush " ++ show n
jvmInstr (Ldc n)     = returnFmt $ "ldc " ++ show n
jvmInstr (IStore l)
    | l <= 3         = returnFmt $ "istore_" ++ show l
    | otherwise      = returnFmt $ "istore " ++ show l
jvmInstr (ILoad l)
    | l <= 3         = returnFmt $ "iload_" ++ show l
    | otherwise      = returnFmt $ "iload " ++ show l

compilePrg :: [Stmt] -> CM (Int, String)
compilePrg ss = do
    program <- mapM compileStmt ss
    let stackSize = if program == [] then 0 else maximum $ map fst program
    let code = concat $ map snd program
    return (stackSize, code)

compileStmt :: Stmt -> CM (Int, String)
compileStmt (SAss x e) = do
    (stack, code1) <- compileExp e
    l <- writeLoc (takeStr x)
    code2 <- jvmInstr (IStore l)
    return (stack, code1 ++ code2)
compileStmt (SExp e) = do
    (stack, code1) <- compileExp e
    code2 <- jvmInstr PrintStream
    return (max stack 2, code1 ++ code2)

compileExp :: Exp -> CM (Int, String)
compileExp (ExpLit n)
    | n == -1                   = jvmInstr (IConstM1) >>= \code -> return (1, code)
    | n >= 0 && n <= 5          = jvmInstr (IConst n) >>= \code -> return (1, code)
    | n >= -128 && n <= 127     = jvmInstr (BiPush n) >>= \code -> return (1, code)
    | n >= -32768 && n <= 32767 = jvmInstr (SiPush n) >>= \code -> return (1, code)
    | otherwise                 = jvmInstr (Ldc n) >>= \code -> return (1, code)
compileExp (ExpVar x) = do
    l <- getLoc (takeStr x)
    jvmInstr (ILoad l) >>= \code -> return (1, code)
compileExp (ExpAdd e1 e2) = do
    (stack1, code1) <- compileExp e1
    (stack2, code2) <- compileExp e2
    code3 <- jvmInstr IAdd
    let stack = computeStackSize stack1 stack2
    let code = if stack1 < stack2
               then code2 ++ code1 ++ code3
               else code1 ++ code2 ++ code3
    return (stack, code)
compileExp (ExpSub e1 e2) = do
    (stack1, code1) <- compileExp e1
    (stack2, code2) <- compileExp e2
    code3 <- jvmInstr ISub
    swap <- returnFmt "swap"
    let stack = computeStackSize stack1 stack2
    let code = if stack1 < stack2
               then code2 ++ code1 ++ swap ++ code3
               else code1 ++ code2 ++ code3
    return (stack, code)
compileExp (ExpMul e1 e2) = do
    (stack1, code1) <- compileExp e1
    (stack2, code2) <- compileExp e2
    code3 <- jvmInstr IMul
    let stack = computeStackSize stack1 stack2
    let code = if stack1 < stack2
               then code2 ++ code1 ++ code3
               else code1 ++ code2 ++ code3
    return (stack, code)
compileExp (ExpDiv e1 e2) = do
    (stack1, code1) <- compileExp e1
    (stack2, code2) <- compileExp e2
    code3 <- jvmInstr IDiv
    swap <- returnFmt "swap"
    let stack = computeStackSize stack1 stack2
    let code = if stack1 < stack2
               then code2 ++ code1 ++ swap ++ code3
               else code1 ++ code2 ++ code3
    return (stack, code)

compile :: Program -> String -> IO String
compile (Prog stmts) filename = do
    ((stackSize, code), env) <- runStateT (compilePrg stmts) initEnv
    let locals = max 1 (Map.size env)
    let className = takeBaseName filename
    return $ ".class public " ++ className ++ "\n" ++
             ".super java/lang/Object\n\n" ++
             ".method public <init>()V\n" ++
             "    aload_0\n" ++
             "    invokespecial java/lang/Object/<init>()V\n" ++
             "    return\n" ++
             ".end method\n\n" ++
             ".method public static main([Ljava/lang/String;)V\n" ++
             ".limit stack " ++ show stackSize ++ "\n" ++
             ".limit locals " ++ show locals ++ "\n" ++
             code ++
             "    return\n" ++
             ".end method\n"
