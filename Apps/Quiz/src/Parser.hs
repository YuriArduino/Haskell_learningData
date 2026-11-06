module Parser (parseInt) where

import           Data.Char              (isDigit, isSpace, ord)
import           Text.Regex.Applicative (many, match, psym)

parseInt :: String -> Maybe Int
parseInt = match (spaces *> parse14 <* spaces)
    where
        spaces = many ( psym isSpace)
        parse14 = (\c -> ord c - 48) <$>
            psym (\c -> isDigit c && c <= '4' && c > '0')
