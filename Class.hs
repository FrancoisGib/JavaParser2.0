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
data Class = Class Name Privacy (Maybe Class) [Class] [Attribute] [Method] LineCount (Maybe Package) [Class] [Dependency]
            | Interface [Maybe Class] [Method] LineCount Package
            | AbstractClass Name Privacy (Maybe Class) [Maybe Class] [Attribute] [Method] LineCount Package [Class]

instance Show Class where
    show (Class name privacy extend implements attributes methods lineCount package nestedClasses dependencies) = "Class\nName: " ++ name ++ "\nPrivacy: " ++ privacy 
                                                                            ++ "\nExtends: " ++ (show extend) ++ "\nImplements: " ++ (show implements)
                                                                            ++ "\nAttributes: [" ++ (showAttributes attributes) ++ "]\nMethods: " ++ (showMethods methods)
                                                                            ++ "\nLine count: " ++ (show lineCount) ++ (showPackage package) ++ "\nDependencies: " ++ (show dependencies)

getMethods :: Class -> [Method]
getMethods (Class _ _ _ _ _ methods _ _ _ _) = methods

getAttributes :: Class -> [Attribute]
getAttributes (Class _ _ _ _ attributes _ _ _ _ _) = attributes

getInnerClasses :: Class -> [Class]
getInnerClasses (Class _ _ _ _ _ _ _ _ innerClasses _) = innerClasses

getDependencies :: Class -> [Dependency]
getDependencies (Class _ _ _ _ _ _ _ _ _ dependencies) = dependencies

getPackage :: Class -> Maybe Package
getPackage (Class _ _ _ _ _ _ _ package _ _) = package

countLines :: Class -> LineCount
countLines classObj = 
        length (getAttributes classObj)
        + foldr (\method acc -> countMethodLines method + acc) 0 (getMethods classObj)
        + (if (getPackage classObj) /= Nothing then 1 else 0)
        + foldr (\innerClass acc -> countLines innerClass + acc) 0 (getInnerClasses classObj)
        + length (getDependencies classObj)
        + 1

classHead :: Parser (Name, Privacy)
classHead = spaceBA privacyP >>= \privacy -> (chaine "class") >> spaceBA nameP >>= \name -> car '{' >> pure (name, privacy)

classP :: Parser Class
classP = do
    package <- packageP
    nextLine
    dependencies <- dependenciesP
    nextLine
    (name, privacy) <- spaceBA classHead <|> pure ("","")
    nextLine
    attributes <- attributesP <|> pure []
    nextLine
    methods <- methodsP <|> pure []
    nextLine
    ((many $ carQuand (\x -> x /= '}')) >> car '}')
    pure (Class name privacy Nothing [] attributes methods package [] dependencies)
