module GlobRegex
(
  globToRegex
  , matchesGlob
) where

import Text.Regex.Posix((=~))

globToRegex :: String -> String
globToRegex glob = '^' : globToRegex' glob ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':cs) = "[^" ++ charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` escapeChars = "\\" ++ [c]
         | otherwise = [c]
  where escapeChars = "\\+*()^$.{}|"

charClass :: String -> String
charClass (']':cs) = ']':globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "Unterminated class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat
