module Lib where

import Data.List
import Data.Char
import Data.String
import Text.Read
import Data.Tuple

------------

getFstT :: (a,b,c,d) -> a
getFstT (a,b,c,d) = a

getSndT :: (a,b,c,d) -> b
getSndT (a,b,c,d) = b

getThdT :: (a,b,c,d) -> c
getThdT (a,b,c,d) = c

getFthT :: (a,b,c,d) -> d
getFthT (a,b,c,d) = d

isOp :: Char -> Bool
isOp c = c == '+' || c == '-' || c == '*' || c == '/' || c == '^'

hasOp :: String -> Bool
hasOp "" = False
hasOp (c:s) = isOp c || hasOp s


---------------

addMoreParr :: String -> String
addMoreParr "" = ""
addMoreParr ('+':s) = "))+((" ++ (addMoreParr s)
addMoreParr ('-':s) = "))-((" ++ (addMoreParr s)
addMoreParr ('^':s) = "^" ++ (addMoreParr s)
addMoreParr ('*':s) = ")*(" ++ (addMoreParr s)
addMoreParr ('/':s) = ")/(" ++ (addMoreParr s)
addMoreParr ('(':s) = "((" ++ (addMoreParr s)
addMoreParr (')':s) = "))" ++ (addMoreParr s)
addMoreParr (c:s) | otherwise = c:(addMoreParr s)

addParr :: String -> String
addParr s = "((((" ++ (addMoreParr (addErrorPreFilter s)) ++ "))))"

addParrAfterNumber :: String -> String -> String
addParrAfterNumber res "" = (reverse res) ++ ")"
addParrAfterNumber res (c:s) | isNumber c = addParrAfterNumber (c:res) s
                             | otherwise = (reverse res) ++ ")" ++ (c:s)


addErrorPreFilter :: String -> String
addErrorPreFilter ('-':s) = ("(0-" ++ addErrorFilter (addParrAfterNumber "" s))
addErrorPreFilter ('.':s) = ("(0." ++ addErrorFilter (addParrAfterNumber "" s))
addErrorPreFilter (c:s) = addErrorFilter (c:s)
addErrorPreFilter "" = ""

addErrorFilter :: String -> String
addErrorFilter "" = ""
addErrorFilter (')':b:s) = ')':(addErrorFilter (b:s))
addErrorFilter (a:b:s)   | (b == '-' && (isNumber a) == False) = a: ("(0-" ++ addErrorFilter (addParrAfterNumber "" s))
                         | (b == '.' && (isNumber a) == False) = a: ("(0." ++ addErrorFilter (addParrAfterNumber "" s))
                         | otherwise = a:(addErrorFilter (b:s))
addErrorFilter (b:s) = b:(addErrorFilter s)

----------------------

takeBetweenParr :: Int -> String -> String -> (String, String)
takeBetweenParr n res (c:s) | c == ')' && n == 0 = ((reverse res), s)
                            | c == ')' && n /= 0 = takeBetweenParr (n - 1) (c:res) s
                            | c == '(' = takeBetweenParr (n + 1) (c:res) s
                            | otherwise = takeBetweenParr n (c:res) s
takeBetweenParr n res "" = ("err parr", "")


doOp :: Char -> Double -> Double -> Maybe Double
doOp '+' x y = Just ( x + y )
doOp '^' x y = Just ( x ** y )
doOp '-' x y = Just ( x - y )
doOp '*' x y = Just ( x * y )
doOp '/' x 0 = Nothing
doOp '/' x y = Just ( x / y )
doOp 'n' x y = Just ( x )
doOp _ x y = Nothing

getOpInfo :: String -> (Char, String, Double, String)
getOpInfo s = (op, n1, n2, end)
            where
                op = fst (parseOperator (s))
                n1 = snd (parseOperator (s))
                n2 = fst (parseNumber "" (snd (parseOperator (s))))
                end = snd (parseNumber "" (snd (parseOperator (s))))

evalExpr :: Double -> String -> Maybe Double
evalExpr (600) "" = Nothing
evalExpr n "" = Just n
evalExpr n ('(':s) = case (evalExpr n (fst (takeBetweenParr 0 "" s))) of
                    Just x -> evalExpr x ((snd (takeBetweenParr 0 "" s)))
                    Nothing -> Nothing
evalExpr (600) (c:s) | isNumber c = evalExpr (fst (parseNumber "" (c:s))) (snd (parseNumber "" (c:s)))
evalExpr n (c:s) | isOp c = case n2 of
                        Just g -> case (doOp op n g) of 
                            Just b -> (evalExpr b end)
                            Nothing -> Nothing
                        Nothing -> Nothing
                 | otherwise = Nothing
                        where
                            op = (getFstT (getOpInfo (c:s)))
                            n1 = (getSndT (getOpInfo (c:s)))
                            n2 = (evalExpr (600) n1)
                            end = (getFthT (getOpInfo (c:s)))



parseNumber :: String -> String -> (Double, String)
parseNumber "" "" = (0, "err")
parseNumber res "" = (read (reverse res) :: Double, "") 
parseNumber res (c:s) | isNumber c || c == '.' = parseNumber (c:res) s
                      | c == '(' = parseNumber res (fst (takeBetweenParr 0 "" s))
                      | c == ')' = parseNumber res s
                      | isOp c = (read (reverse res) :: Double, (c:s)) 
                      | otherwise = (0, "err")

parseOperator :: String -> (Char, String)
parseOperator "" = ('n', "")
parseOperator (c:s) | isOp c = (c, s)
                    | c == '(' || c == ')' = parseOperator s
                    | otherwise = ('$', s)
