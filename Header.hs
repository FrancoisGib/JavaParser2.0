module Header (Dependency, Package, packageP, dependenciesP, showPackage) where

import Parser
import Lib

type Package = String

packageNotAllowedWords = ["abstract", "assert", "boolean", "break", "byte", "case",
                          "catch", "char", "class", "const", "continue", "default",
                          "do", "double", "else", "enum", "extends", "final", "finally",
                          "float", "for", "goto", "if", "implements", "import", "instanceof",
                           "int", "interface", "long", "native", "new", "package", "private",
                           "protected", "public", "return", "short", "static", "strictfp",
                           "super", "switch", "synchronized", "this", "throw", "throws",
                           "transient", "try", "void", "volatile", "while"]

packageAllowedFirstCharacters = ['a'..'z']
packageAllowedCharactersExceptFirst = ['A'..'Z']++['a'..'z']++['0'..'9']
packageLastChar = '*'

packageP :: Parser (Maybe Package)
packageP = do
         spaceBA $ chaine "package"
         packageName <- some $ carQuand (\x -> x /= ';')
         spaceB $ car ';'
         pure $ Just packageName

showPackage :: (Maybe Package) -> String
showPackage Nothing = ""
showPackage (Just package) = "\nPackage: " ++ package

data Dependency = Dependency String | Package String deriving Show

-- PackageDep
notContainsTwoPointsSucc :: [Char] -> Bool
notContainsTwoPointsSucc [_] = True
notContainsTwoPointsSucc ('.':'.':_) = False
notContainsTwoPointsSucc (x:xs) = True && notContainsTwoPointsSucc xs

notContainsTwoPointsSuccAll :: [String] -> Bool
notContainsTwoPointsSuccAll = foldr (\x acc -> if notContainsTwoPointsSucc x then acc else False) True

splitByPoints :: String -> [String]
splitByPoints l = split "" l
   where
      split acc [] = [acc]
      split acc ('.':xs) = [acc] ++ split [] xs
      split acc (x:xs) =  split (acc ++ [x]) xs

wordFirstCharAllowed :: String -> Bool
wordFirstCharAllowed (x:xs) = elem x packageAllowedFirstCharacters

wordCharsAllowed :: String -> Bool
wordCharsAllowed str = foldr (\c acc -> if (elem c packageAllowedCharactersExceptFirst) then acc else False) True str

wordAllowed :: String -> Bool
wordAllowed x = if elem x packageNotAllowedWords then False else True

getBeforeLast :: [a] -> a
getBeforeLast [] = error "Empty list"
getBeforeLast [_] = error "Need at least two elements in the list"
getBeforeLast (x:_:[]) = x
getBeforeLast (_:xs) = getBeforeLast xs

checkPackagePath :: [String] -> Bool
checkPackagePath ["*"] = True
checkPackagePath [_] = False
checkPackagePath (x:xs) = notContainsTwoPointsSucc x && wordFirstCharAllowed x && wordCharsAllowed x && wordAllowed x && checkPackagePath xs

packageDependencyP :: Parser Dependency
packageDependencyP = spaceB (some $ carQuand (\c -> c /= ';')) >>= \package -> car ';' >> 
                     if checkPackagePath (splitByPoints package) then pure (Package $ (getBeforeLast $ splitByPoints package)) else empty

-- Dependency

getLast :: [a] -> a
getLast [] = error "Empty list"
getLast [x] = x
getLast (x:xs) = getLast xs

singleDependencyP :: Parser Dependency
singleDependencyP = spaceB (some $ carQuand (\c -> c /= ';')) >>= \package -> car ';' >>
                     if foldr (\x acc -> if notContainsTwoPointsSucc x && wordFirstCharAllowed x && wordCharsAllowed x && wordAllowed x then True else acc) True (splitByPoints package)
                     then pure (Dependency $ (getLast $ splitByPoints package)) else empty

dependencyP :: Parser Dependency
dependencyP = spaceBA $ chaine "import" >> (packageDependencyP <|> singleDependencyP)

dependenciesP :: Parser [Dependency]
dependenciesP = many (dependencyP >>= \dep -> nextLine >> pure dep)