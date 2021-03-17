import Data.List
import Data.Char
import Data.String
import System.Environment
import System.Exit
import System.Posix
import Text.Read

type  Parser a = String  -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar a [] = Nothing
parseChar a (c:s) | a == c = Just (a, s)
                  | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] s = Nothing
parseAnyChar a [] = Nothing
parseAnyChar (c:s) b | parseChar c b /= Nothing =  parseChar c b
                     | otherwise = parseAnyChar s b

parseOr :: Parser a -> Parser a -> Parser a
parseOr f g s = case (f s) of
              Nothing -> g s
              otherwise -> f s

parseAnd  ::  Parser a -> Parser b -> Parser (a,b)
parseAnd f g s = case (f s) of
                Nothing -> Nothing
                Just x -> case (g (snd x)) of
                    Nothing -> Nothing
                    Just y -> Just ((fst x, fst y), snd y)

parseAndWith  :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith m f g s = case (f s) of
                Nothing -> Nothing
                Just x -> case (g (snd x)) of
                    Nothing -> Nothing
                    Just y -> Just (m (fst x) (fst y), snd y)

parseMany  ::  Parser a -> Parser [a]
parseMany f s = case (f s) of
                Nothing -> Just ("", s)
                Just x -> case (f (snd x)) of
                    Nothing -> Just ([(fst x)], s)
                    Just y -> Just ([(fst x)] ++ (fst (y)), snd (y))


main = do
    args <- getArgs
    exitWith (ExitSuccess)