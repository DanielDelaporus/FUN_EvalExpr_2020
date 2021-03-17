import Data.List
import Data.Char
import Data.String
import System.Environment
import System.Exit
import System.Posix
import Text.Read

data Expr = Num Double | Add Expr Expr 
                       | Sub Expr Expr
                       | Mul Expr Expr
                       | Div Expr Expr
                       | Err Expr Expr
          deriving (Eq, Show)

-- | Extract the 'fst' of a triple.
fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

-- | Extract the 'snd' of a triple.
snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

-- | Extract the third element of a triple.
thd4 :: (a,b,c, d) -> c
thd4 (a,b,c,d) = c

-- | Extract the final element of a triple.
fth4 :: (a,b,c, d) -> d
fth4 (a,b,c,d) = d




isOp :: Char -> Bool
isOp c = c == '+' || c == '-' || c == '*' || c == '/' 

hasOp :: String -> Bool
hasOp "" = False
hasOp (c:s) = isOp c || hasOp s




addMoreParr :: String -> String
addMoreParr "" = ""
addMoreParr (c:s) | c == '+' = "))+((" ++ (addMoreParr s)
                  | c == '-' = "))-((" ++ (addMoreParr s)
                  | c == '*' = ")))*(((" ++ (addMoreParr s)
                  | c == '/' = ")))/(((" ++ (addMoreParr s)
                  | otherwise = c:(addMoreParr s)

addParr :: String -> String
addParr s = "((((" ++ (addMoreParr s) ++ "))))"

addErrorFilter :: String -> String
addErrorFilter "" = ""
addErrorFilter (a:b:s) | (b == '-' && (isNumber a) == False) = a:("O-" ++ addErrorFilter s)
                       | (b == '.' && (isNumber a) == False) = a:("O." ++ addErrorFilter s)
                       | otherwise = a:(addErrorFilter (b:s))

takeBetweenParr :: Int -> String -> String -> String
takeBetweenParr n res (c:s) | c == ')' && n == 0 = (reverse res) ++ s
                            | c == ')' && n /= 0 = takeBetweenParr (n - 1) (c:res) s
                            | c == '(' = takeBetweenParr (n + 1) (c:res) s
                            | otherwise = takeBetweenParr n (c:res) s
takeBetweenParr n res "" = "err parr"


doOp :: Char -> Double -> Double -> Double
doOp '+' x y = x + y
doOp '-' x y = x - y
doOp '*' x y = x * y
doOp '/' x y = x / y
doOp 'n' x y = x
doOp _ x y = 99999 --ERR

getOpInfo :: String -> (Char, Double, String, String)
getOpInfo s = (op, n1, n2, end)
            where
                op = fst (parseOperator (snd (parseNumber "" s)))
                n1 = fst (parseNumber "" s)
                n2 = snd (parseOperator (snd (parseNumber "" s)))
                end = ""--snd (parseNumber "" (snd (parseOperator (snd (parseNumber "" s)))))

evalExpr :: String -> Double
evalExpr "" = 99999 --ERR
evalExpr (c:s) | c == '(' = evalExpr (takeBetweenParr 0 "" s)
               | isNumber c = case (hasOp (c:s)) of
                            True -> evalExpr (show (doOp (fst4 (getOpInfo (c:s))) (snd4 (getOpInfo (c:s))) (evalExpr (thd4 (getOpInfo (c:s))))))
                            False -> read ((c:s)) :: Double
               | otherwise = 0

parseNumber :: String -> String -> (Double, String)
parseNumber "" "" = (0, "")
parseNumber res "" = (read (reverse res) :: Double, "") 
parseNumber res (c:s) | isNumber c || c == '.' = parseNumber (c:res) s
                      | c == '(' = parseNumber res (takeBetweenParr 0 "" s)
                      | c == ')' = parseNumber res s
                      | isOp c = (read (reverse res) :: Double, (c:s)) 
                      | otherwise = (99999, "")

parseOperator :: String -> (Char, String)
parseOperator "" = ('n', "")
parseOperator (c:s) | c == '+' = (c, s)
                    | c == '-' = (c, s)
                    | c == '*' = (c, s)
                    | c == '/' = (c, s)
                    | c == '(' || c == ')' = parseOperator s
                    | otherwise = ('$', s)

main = do
    args <- getArgs
    case ((length args) /= 1) of
        True -> do
            putStrLn "invalid number of args"
            exitWith (ExitFailure 84)
        False -> do
            return()

    putStrLn (show (evalExpr (addParr ((filter (/=' ') (args !! 0))))))
    exitWith (ExitSuccess)