module Parser (parseInt) where

import Text.Regex.Applicative (match, many, psym)
import Data.Char (isSpace, ord, isDigit)

parseInt :: String -> Maybe Int
parseInt = match (spaces *> parse14 <* spaces)
    where
        spaces = many ( psym isSpace)
        parse14 = (\c -> ord c - 48) <$>
            psym (\c -> isDigit c && c <= '4' && c > '0')
