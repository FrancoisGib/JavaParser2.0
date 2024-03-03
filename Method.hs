module Method (Method,  parseMethodImplementation, methodsP, abstractMethodP, countMethodLines, showMethods, (==)) where

import Parser
import Lib

-- Param
type Param = (Type, Name)

showParam :: Param -> String
showParam (typeName, name) = '(' : name ++ ": " ++ (show typeName) ++ ")"

showAllParams :: [Param] -> String
showAllParams params = "[" ++ foldr (\param acc -> if isLast param params then (showParam param) ++ acc else (showParam param) ++ ", " ++ acc) "" params ++ "]"
                    where
                        isLast param [x] = x == param
                        isLast param (x:xs) = x /= param && isLast param xs

paramP :: Parser Param
paramP = (\typeName name -> (typeName, name)) <$> spaceBA typeP <*> spaceA nameP

-- Method
data Method = Method Name Privacy Type [Param] LineCount
            | AbstractMethod Name Privacy Type [Param] LineCount

instance Show Method where
    show (Method name privacy typeName params lineCount) = "Method (name: " ++ name ++ ", privacy: " 
                                                        ++ privacy ++ ", return type: " ++ (show typeName) ++ ", params: " ++ 
                                                        showAllParams params ++ ", line count: " ++ (show lineCount) ++ ")"
    show method = "Abstract " ++ show method

instance Eq Method where
    (==) (Method _ _ _ _ _) (AbstractMethod _ _ _ _ _) = False
    (==) (Method a1 b1 c1 d1 e1) (Method a2 b2 c2 d2 e2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2
    (==) (AbstractMethod a1 b1 c1 d1 e1) (AbstractMethod a2 b2 c2 d2 e2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2

parseMethodImplementation :: Parser LineCount
parseMethodImplementation = spaceB (car '{') >> wellParenthesized 0 1
                        where
                            wellParenthesized lineCount 0 = (spaceB (car '\n') >> pure (lineCount + 1)) <|> pure lineCount
                            wellParenthesized lineCount cpt = 
                                (car '}' >> wellParenthesized 0 (cpt - 1) >>= \count -> pure (lineCount + count))
                                <|> (car '{' >> wellParenthesized 0 (cpt + 1) >>= \count -> pure (lineCount + count))
                                <|> (car '\n' >> wellParenthesized 0 cpt >>= \count -> pure (lineCount + 1 + count))
                                <|> (carQuand (\x -> not (elem x ['\n', '{', '}'])) >> wellParenthesized 0 cpt)
                                

methodP :: Parser Method
methodP = (\privacy typeName name _ params _ lineCount -> Method name privacy typeName params lineCount) 
    <$> spaceBA word <*> spaceA typeP <*> spaceA nameP <*> spaceA (car '(') <*> (zeroOrMoreComma paramP) <*> spaceBA (car ')') <*> parseMethodImplementation

abstractMethodP :: Parser Method
abstractMethodP = (\privacy typeName name _ params _ lineCount -> Method name privacy typeName params lineCount) 
    <$> spaceBA word <*> spaceA typeP <*> spaceA word <*> spaceA (car '(') <*> (zeroOrMoreComma paramP) <*> spaceBA (car ')') <*> parseMethodImplementation

countMethodLines :: Method -> LineCount
countMethodLines (Method _ _ _ _ lineCount) = lineCount 
countMethodLines (AbstractMethod _ _ _ _ lineCount) = lineCount 

showMethods :: [Method] -> String
showMethods methods = "[" ++ foldr (\method acc -> if isLast method methods then (show method) ++ acc else (show method) ++ "\n\t" ++ acc) "" methods ++ "]"
                    where
                        isLast method [x] = x == method
                        isLast method (x:xs) = x /= method && isLast method xs

methodsP :: Parser [Method]
methodsP = many ((methodP <|> abstractMethodP) >>= \method ->(many $ car '\n') >> pure method)