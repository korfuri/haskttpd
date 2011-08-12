module Haskttpd.Handler (
                         handle
                        ) where

    import System.IO
    import System.IO.Error
    import qualified Network.Socket.Internal as NSI
    import qualified Control.Exception as C (catch)
    import Control.Monad.Reader
    import Haskttpd.Config
    import Haskttpd.Parser
    import Directory

    data HttpReply = HttpReply {
          repversion :: String,
          repcode :: Int,
          repmsg :: String,
          repheaders :: [(String, String)],
          repbody :: String
        } deriving (Eq)

    httpHeadersToString :: [(String, String)] -> String
    httpHeadersToString hdrs = foldl step "" hdrs
        where step s (h, c) = s ++ h ++ ": " ++ c ++ "\n"

    httpReplyToString :: HttpReply -> String
    httpReplyToString r =
      repversion r ++ " " ++ (show $ repcode r) ++ " " ++ repmsg r ++ "\n" ++
      (httpHeadersToString $ repheaders r) ++ "\n" ++
      repbody r ++ "\n"

    instance Show HttpReply where
        show = httpReplyToString

    defaultHeaders :: [(String, String)]
    defaultHeaders = [("server", "haskttpd")]

    generateResponseInError :: HttpRequest -> IOError -> IO HttpReply
    generateResponseInError q e
        | isDoesNotExistError e = return $ errorReply q 404 "File Not Found"
        | isPermissionError e = return  $ errorReply q 403 "Permission denied"
        | isAlreadyInUseError e = return  $ errorReply q 500 "Already in use"
        | isIllegalOperation e = return  $ errorReply q 500 "Illegal Operation"
        | isUserError e = return  $ errorReply q 500 "User Error"
        | isEOFError e = return  $ errorReply q 500 "EOF Error"
        | otherwise = return $ errorReply q 500 "An internal error occurred"
          where
            getMessageFromErrorCode 500 = "Internal Error"
            getMessageFromErrorCode 404 = "Not Found"
            getMessageFromErrorCode 403 = "Permission Denied"
            getMessageFromErrorCode _ = "Error"
            errorReply :: HttpRequest -> Int -> String -> HttpReply
            errorReply q code message = HttpReply (reqversion q) code (getMessageFromErrorCode code) defaultHeaders message


    generateResponseFile :: HttpRequest -> String -> ReaderT Config IO HttpReply
    generateResponseFile q file = do
      fileh <- liftIO $ openBinaryFile file ReadMode
      s <- liftIO $ hGetContents fileh
      return $ HttpReply (reqversion q) 200 "OK" defaultHeaders s

    generateResponseIndex :: HttpRequest -> String -> ReaderT Config IO HttpReply
    generateResponseIndex q file = generateResponseFile q $ file ++ "/index.html"

    generateResponseAutoIndex :: HttpRequest -> String -> ReaderT Config IO HttpReply
    generateResponseAutoIndex q dir = do
      files <- liftIO $ getDirectoryContents dir
      return $ HttpReply (reqversion q) 200 "OK" defaultHeaders $ buildIndex files
        where
          buildIndex = foldl step "<html><body><h1>Index</h1>"
          step base file = base ++ "<br><a href=\"" ++ (reqressource q) ++ "/" ++ file ++ "\">" ++ file ++ "</a>"

    attemptAllResponseGenerators :: HttpRequest -> ReaderT Config IO HttpReply
    attemptAllResponseGenerators q = do
      conf <- ask
      let prependPath = getValueOrEmpty conf "DocumentRoot"
      let file = prependPath ++ reqressource q
          
      isDir <- liftIO $ Directory.doesDirectoryExist file
      if isDir
       then
         liftIO $ Prelude.catch
                    (do runReaderT (generateResponseIndex q file) conf)
                    (f q file conf)
       else do
         generateResponseFile q file
        where
          f q file conf _ = do runReaderT (generateResponseAutoIndex q file) conf

    generateResponse :: HttpRequest -> Int -> (NSI.HostAddress, NSI.PortNumber) -> ReaderT Config IO HttpReply
    generateResponse q _ _ = do
      conf <- ask
      liftIO $ do
        Prelude.catch (do runReaderT (attemptAllResponseGenerators q) (conf)) $ generateResponseInError q

    handle :: Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> ReaderT Config IO ()
    handle h cid connxInfos = do
      req <- liftIO $ parseRequestFromStream h
      resp <- generateResponse req cid connxInfos
      liftIO $ hPutStr h (show resp)
