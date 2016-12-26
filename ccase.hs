--    Copyright 2017 Allan Psiocbyte psicobyte@gmail.com
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>



import System.Environment
import Data.Char
import Data.List


haycadena :: [String] -> String
haycadena [] = ayuda
haycadena (x:xs) = modificadores (x:xs)


empiezaguion :: String -> Bool
empiezaguion (x:xs) =
    if x == '-'
    then True
    else False


modificadores :: [String] -> String
modificadores (x:xs) =
    if empiezaguion x
    then procesa (x:xs)
    else map toUpper $ intercalate " " (x:xs)


procesa :: [String] -> String
procesa (x:xs)
    | x == "-u" = map toUpper $ intercalate " " (xs)
    | x == "-l" = map toLower $ intercalate " " (xs)
    | x == "-c" = scanl (capitaliza) ' ' $ intercalate " " (xs)
    | x == "-h" = ayuda
    | otherwise = "Invalid option \"" ++ x ++ "\". Try -u, -l, -c or -h"


capitaliza :: Char -> Char -> Char
capitaliza ' ' x = toUpper x
capitaliza '\n' x = toUpper x
capitaliza _ x = toLower x


ayuda = "Change case of a string.\n\
\\n\
\use: case [-u|-l|-c|-h] string\n\
\-u: Uppercase.\n\
\-l: Lowercase.\n\
\-c: Capitalize.\n\
\-h: This message."


main = do
    args <- getArgs

    entrada <- getContents

    putStrLn $ haycadena $ args ++ entrada:[]














