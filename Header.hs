module Header (Dependency, Package, packageP, packageDependencyP) where

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

packageP :: Parser Package
packageP = spaceBA $ chaine "package" >> (some $ carQuand (\x -> x /= ';')) >>= \packageName -> spaceB (car ';' >> nextLine) >> pure packageName

data Dependency = Dependency String | PackageDep [String] deriving Show

packageDependencyP :: Parser Dependency
packageDependencyP = spaceB (some $ carQuand (\c -> c /= ';')) >>= \package -> (car ';' >> convert package)
   where
      notContainsTwoPointsSucc ['.'] = False
      notContainsTwoPointsSucc ('.':'.':xs) = False
      notContainsTwoPointsSucc (x:y:xs) = True && notContainsTwoPointsSucc (y:xs)
      notContainsTwoPointsSucc _ = True
      notContainsTwoPointsSuccAll p = foldr (\x acc -> if notContainsTwoPointsSucc x then acc else False) True (pack p)
      splitByPoints acc [] = [acc]
      splitByPoints acc ('.':xs) = [acc] ++ splitByPoints [] xs
      splitByPoints acc (x:xs) =  splitByPoints (acc ++ [x]) xs
      allWordsAllowed [] _ = True
      allWordsAllowed (x:xs) l = if elem x l then False else True && allWordsAllowed xs l
      allWordsFirstCharAllowed l = foldr (\(x:xs) acc -> if elem x packageAllowedFirstCharacters then acc else False) True l
      allWordsCharsAllowed l = foldr (\x acc -> if (foldr (\c acc2 -> if (elem c packageAllowedCharactersExceptFirst) then acc2 else False) True (tail x)) then acc else False) True l
      convert p =  if allWordsAllowed (pack p) packageNotAllowedWords && allWordsFirstCharAllowed (pack p) && allWordsCharsAllowed (pack p) && notContainsTwoPointsSucc p
         then pure (PackageDep $ pack p) else empty
      pack p = (splitByPoints [] p)

            

--dependencyP :: Parser Dependency
--dependencyP = spaceAB $ chaine "import" >> 