module Haskttpd.Parser (
                        parseRequestFromStream,
                        HttpRequest,
                        reqmethod,
                        reqressource,
                        reqversion,
                        reqheaders,
                        reqbody
                       ) where

    import System.IO
    import Text.ParserCombinators.Parsec

    data HttpRequest = HttpRequest {
          reqmethod :: String,
          reqressource :: String,
          reqversion :: String,
          reqheaders :: [(String, String)],
          reqbody :: String
        } deriving (Eq, Show)

    httpBNF :: GenParser Char st HttpRequest
    httpBNF = do
      method <- word
      whitespace
      ressource <- ressourceBNF
      whitespace
      version <- word
      eol
      h <- many headerBNF
      eol
--      body <- bodyBNF
      return $ HttpRequest method ressource version h ""

    word = many1 (noneOf " \r\n")
    
    ressourceBNF = word
    
    headerString = many1 (noneOf ": \r\n\t")
    
    headerBNF = do
      h <- headerString
      char ':'
      whitespace
      v <- many1 (noneOf "\r\n")
      eol
      return (h, v)

    whitespace = many1 (char ' ')

    eol = try (string "\n\r")
          <|> try (string "\r\n")
          <|> string "\n"
          <|> string "\r"

    bodyBNF = do
      content <- many nonEmptyLine
      return $ unlines content
        where
          nonEmptyLine = do
                c <- many1 (noneOf "\r\n")
                eol
                return c

    parseRequestFromStream :: Handle -> IO HttpRequest
    parseRequestFromStream h = do
      str <- hGetContents h
--      putStrLn $ "\n\n\n\nParsing :\n" ++ (take 50 str) ++ "\n"
      case parse httpBNF "fail" str of
        Left msg -> do
--          putStrLn (show msg)
          return $ HttpRequest "GET" "/" "HTTP/1.1" [("host", "localhost")] ""
        Right req -> return req

