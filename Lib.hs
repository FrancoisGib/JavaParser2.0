module Lib (word, spaceB, spaceA, spaceBA, notChaineInArrayP, trimEnd, Name, nameP, Privacy, privacyP, Type, typeP, LineCount, zeroOrMoreComma, nextLine) where

import Parser

word :: Parser String
word = (some $ carQuand (`elem` ['a'..'z']++['A'..'Z']))

space :: Parser ()
space = (many $ (car ' ' <|> car '\t')) >> pure ()

spaceB :: Parser a -> Parser a
spaceB p = space >> p

spaceA :: Parser a -> Parser a
spaceA p = p >>= \res -> space >> pure res

spaceBA :: Parser a -> Parser a
spaceBA p = space >> spaceA p

chaineInArrayP :: [String] -> Parser (Maybe String)
chaineInArrayP [] = empty
chaineInArrayP (x:xs) = (chaine x >> pure (Just x)) <|> chaineInArrayP xs

notChaineInArrayP :: [Char] -> [String] -> Parser String
notChaineInArrayP terms [] = some $ carQuand (\c -> not $ elem c terms)
notChaineInArrayP terms (x:xs) = (chaine x >> empty) <|> notChaineInArrayP terms xs

firstClassNameAcceptedCharacters = ['a'..'z']++['A'..'Z']++['_','$'];
classNameAcceptedCharactersExceptFirst = ['a'..'z']++['A'..'Z']++['0'..'9']++['_','$'];

classNameP :: Parser String
classNameP = do
            firstChar <- carQuand (`elem` firstClassNameAcceptedCharacters)
            secondeNamePart <- many $ carQuand (`elem` classNameAcceptedCharactersExceptFirst)
            pure (firstChar : secondeNamePart)

-- Name
type Name = String

nameP :: Parser String
nameP = spaceB (some $ carQuand (`elem` ['a'..'z']++['A'..'Z']++['0'..'9']++['_']))

-- Privacy
type Privacy = String

privacyP :: Parser String
privacyP = chaine "public" <|> chaine "private" <|> chaine "protected"

-- Type
data Type = Primitive String | NonPrimitive String deriving Eq

primitiveTypes = ["byte", "short", "int", "long", "float", "double", "boolean", "char"]

typeP :: Parser Type
typeP = (NonPrimitive <$> classNameP) <|> (chaineInArrayP primitiveTypes >>= \res -> if res == Nothing then empty else pure (Primitive $ myFromJust res))
      where
         myFromJust (Just r) = r

instance Show Type where
   show (NonPrimitive typeName) = typeName
   show (Primitive typeName) = typeName



type LineCount = Int


zeroOrMoreComma :: Parser a -> Parser [a]
zeroOrMoreComma p = (p >>= \res -> (car ',' >> zeroOrMoreComma p >>= \others -> pure (res : others)) <|> pure [res]) <|> pure []

nextLine :: Parser Int
nextLine = (many $ car '\n') >>= \l -> pure (length l)


trimEnd :: String -> String
trimEnd [] = ""
trimEnd " " = ""
trimEnd (' ':xs) = if allSpaceAfter xs then "" else ' ' : trimEnd xs
   where
         allSpaceAfter [] = True
         allSpaceAfter (' ':xs) = True && allSpaceAfter xs
         allSpaceAfter (x:xs) = False
trimEnd (x:xs) = x : trimEnd xs
 
commentAsterisque :: Parser ()
commentAsterisque = (chaine "/**" <|> chaine "/*") >> parseUntilEnd >> pure ()
   where
      parseUntilEnd = (many (carQuand (\x -> x /= '/'))) >>= \asterisk -> if last asterisk == '*' then (car '/') else parseUntilEnd
      last [x] = x
         last (_:xs) = last xs

commentSlash :: Parser ()
commentSlash = chaine "//" >> (many (carQuand (\x -> x /= '\n'))) >> pure ()

parseComments :: Parser ()
parseComments = esp >> (parseComments1 <|> parseComments2 <|> pure ())
