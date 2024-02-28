module Class (Class, classP, countParsedLines) where

import Parser
import Method
import Lib
import Header

-- Attribute 
type Static = Bool
type Final = Bool
type Attribute = (Name, Type, Privacy, Static, Final, Maybe String)

attributeP :: Parser Attribute
attributeP = do
            privacy <- spaceBA privacyP
            static <- (spaceA $ chaine "static" >> pure True) <|> pure False
            final <- (spaceA $ chaine "final" >> pure True) <|> pure False
            attributeType <- spaceA typeP
            name <- spaceA nameP
            defaultValue <- (spaceBA (car '=') >> (some $ carQuand (\x -> x /= ';')) >>= \value -> car ';' >> pure (Just $ trimEnd value)) 
                            <|> (spaceBA $ car ';' >> pure Nothing)
            pure (name, attributeType, privacy, static, final, defaultValue)

attributesP :: Parser [Attribute]
attributesP = many (attributeP >>= \attribute -> nextLine >> pure attribute)

showAttribute :: Attribute -> String
showAttribute (name, attributeType, privacy, static, final, value) = '(' : name ++ ", " ++ (show attributeType) ++ ", " ++ privacy ++ 
                                                                    staticDisplay static ++ finalDisplay final ++ (valueDisplay value) ++ ")"
        where
            valueDisplay Nothing = ""
            valueDisplay (Just value) = ", " ++ value
            staticDisplay False = ""
            staticDisplay True = ", static"
            finalDisplay False = ""
            finalDisplay True = ", final"

showAttributes :: [Attribute] -> String
showAttributes [] = ""
showAttributes [x] = showAttribute x
showAttributes (x:xs) = showAttribute x ++ ", " ++ (showAttributes xs)

-- Class
data Class = Class Name Privacy (Maybe Class) [Maybe Class] [Attribute] [Method] LineCount Package [Class] [Dependency]
            | Interface [Maybe Class] [Method] LineCount Package
            | AbstractClass Name Privacy (Maybe Class) [Maybe Class] [Attribute] [Method] LineCount Package [Class]

instance Show Class where
    show (Class name privacy extend implements attributes methods lineCount package nestedClasses dependencies) = "Class\nName: " ++ name ++ "\nPrivacy: " ++ privacy 
                                                                            ++ "\nExtends: " ++ (show extend) ++ "\nImplements: " ++ (show implements)
                                                                            ++ "\nAttributes: [" ++ (showAttributes attributes) ++ "]\nMethods: " ++ (showMethods methods)
                                                                            ++ "\nLine count: " ++ (show lineCount) ++ "\nPackage: " ++ package ++ "\nDependencies: " ++ (show dependencies)

classP :: Parser Class
classP = packageDependencyP >>= \dep -> pure (Class "" "" Nothing [] [] [] 0 "" [] [dep])

countParsedLines :: Maybe Class -> Int
countParsedLines (Just (Class _ _ _ _ _ _ lineCount _ _ _)) = lineCount
countParsedLines (Just (Interface _ _ lineCount _)) = lineCount
countParsedLines (Just (AbstractClass _ _ _ _ _ _ lineCount _ _)) = lineCount
countParsedLines _ = 0