import Parser
import System.IO
import System.Environment
import Class

parse :: Parser Class -> String -> Class
parse p chaine = case runParser p chaine of
                Just (res, []) -> res
                Just (res, content) -> error ("Unable to parse since line " ++ (show (countParsedLines (Just res) + 1)) ++ "\nContent not parsed:\n" ++ content)
                Nothing -> error "The file you want to parse contains incorrect patterns"

main :: IO ()
main = do
    args <- getArgs
    result <- tryReadFile (head args)
    case result of
        Just content -> do
            let parsed = parse classP content
            putStrLn (show parsed)
        Nothing -> putStrLn "Erreur : Impossible de lire le fichier."

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fileName = do
    content <- readFile fileName
    return (Just content)
