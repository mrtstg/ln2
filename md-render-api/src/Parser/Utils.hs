module Parser.Utils
  ( between'
  , space'
  , space''
  , asterisk'
  , asterisk''
  , codeBlock
  , transliterateCharacter
  , tableDivider
  , tableColumn
  ) where

import           Data.Attoparsec.Combinator (lookAhead, many1)
import           Data.Attoparsec.Text
import           Data.Char                  (isLetter, isNumber)

transliterateCharacter :: Char -> String
transliterateCharacter 'а' = "a"
transliterateCharacter 'б' = "b"
transliterateCharacter 'в' = "v"
transliterateCharacter 'г' = "g"
transliterateCharacter 'д' = "d"
transliterateCharacter 'е' = "e"
transliterateCharacter 'ё' = "e"
transliterateCharacter 'ж' = "zh"
transliterateCharacter 'з' = "z"
transliterateCharacter 'и' = "i"
transliterateCharacter 'й' = "i"
transliterateCharacter 'к' = "k"
transliterateCharacter 'л' = "l"
transliterateCharacter 'м' = "m"
transliterateCharacter 'н' = "n"
transliterateCharacter 'о' = "o"
transliterateCharacter 'п' = "p"
transliterateCharacter 'р' = "r"
transliterateCharacter 'с' = "s"
transliterateCharacter 'т' = "t"
transliterateCharacter 'у' = "y"
transliterateCharacter 'ф' = "f"
transliterateCharacter 'х' = "h"
transliterateCharacter 'ц' = "c"
transliterateCharacter 'ч' = "ch"
transliterateCharacter 'ш' = "sh"
transliterateCharacter 'щ' = "sch"
transliterateCharacter 'ъ' = ""
transliterateCharacter 'ы' = "i"
transliterateCharacter 'ь' = ""
transliterateCharacter 'э' = "e"
transliterateCharacter 'ю' = "yu"
transliterateCharacter 'я' = "ya"
transliterateCharacter ' '   = " "
transliterateCharacter s | isLetter s || isNumber s = [s]
                         | otherwise = []

between' :: Parser a -> Parser b -> Parser c -> Parser [c]
between' a b c = a *> manyTill c (try b)

space' :: Parser String
space' = choice [try $ many1 (char ' '), return ""]

space'' :: Parser String
space'' = many1 (char ' ')

asterisk' :: Parser String
asterisk' = count 1 $ char '*'

asterisk'' :: Parser String
asterisk'' = count 2 $ char '*'

codeBlock :: Parser String
codeBlock = count 3 (char '`')

tableDivider :: Parser String
tableDivider = many1 (choice [char '|', char '-'])

tableColumn :: Parser Char
tableColumn = char '|' <* space'
