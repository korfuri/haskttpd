module Httphandler (
                    handle
                   ) where

    import System.IO
    import qualified Network.Socket.Internal as NSI
    import Text.ParserCombinators.Parsec
    import qualified Control.Exception as C (catch)
    import Control.Monad.Reader
    import HttpConfig

    data HttpRequest = HttpRequest {
          reqmethod :: String,
          reqressource :: String,
          reqversion :: String,
          reqheaders :: [(String, String)],
          reqbody :: String
        } deriving (Eq, Show)

    data HttpReply = HttpReply {
          repversion :: String,
          repcode :: Int,
          repmsg :: String,
          repheaders :: [(String, String)],
          repbody :: String
        } deriving (Eq)

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
      putStrLn $ "\n\n\n\nParsing :\n" ++ (take 50 str) ++ "\n"
      case parse httpBNF "fail" str of
        Left msg -> do
          putStrLn (show msg)
          return $ HttpRequest "GET" "/" "HTTP/1.1" [("host", "localhost")] ""
        Right req -> return req

    httpHeadersToString hdrs = foldl step "" hdrs
        where step s (h, c) = s ++ h ++ ": " ++ c ++ "\n"

    httpReplyToString :: HttpReply -> String
    httpReplyToString r =
      repversion r ++ " " ++ (show $ repcode r) ++ " " ++ repmsg r ++ "\n" ++
      (httpHeadersToString $ repheaders r) ++ "\n" ++
      repbody r ++ "\n"

    instance Show HttpReply where
        show = httpReplyToString

    generateResponse :: String -> HttpRequest -> Int -> (NSI.HostAddress, NSI.PortNumber) -> IO HttpReply
    generateResponse prependPath q _ _ = Prelude.catch (do
      let file = prependPath ++ reqressource q
      fileh <- Prelude.catch (openFile file ReadMode) (\_ -> ioError $ userError "404 Not Found")
      s <- hGetContents fileh
--      hClose fileh
      return $ HttpReply (reqversion q) 200 "OK" [("server", "haskttpd")] s
                                   ) $ \_ ->
                                    return $ HttpReply (reqversion q) 404 "Not Found" [("server", "haskttpd")] "lolilol 404"

    handle' :: Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> ReaderT Config IO ()
    handle' h cid connxInfos = do
      conf <- ask
      let prependPath = getValueOrEmpty conf "DocumentRoot"
      req <- liftIO $ parseRequestFromStream h
      liftIO $ putStrLn (show req)
      resp <- liftIO $ generateResponse prependPath req cid connxInfos
      liftIO $ putStrLn (show resp)
      liftIO $ hPutStr h (show resp)

    handle :: Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> IO ()
    handle h cid connxInfos = do
      runReaderT (handle' h cid connxInfos) $ Config [("DocumentRoot", "/home/korfuri/")]
