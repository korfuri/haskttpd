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


    httpHeadersToString hdrs = foldl step "" hdrs
        where step s (h, c) = s ++ h ++ ": " ++ c ++ "\n"

    httpReplyToString :: HttpReply -> String
    httpReplyToString r =
      repversion r ++ " " ++ (show $ repcode r) ++ " " ++ repmsg r ++ "\n" ++
      (httpHeadersToString $ repheaders r) ++ "\n" ++
      repbody r ++ "\n"

    instance Show HttpReply where
        show = httpReplyToString

    generateResponseInError q e
        | isDoesNotExistError e = return $ HttpReply (reqversion q) 504 "Not Found" [("server", "haskttpd")] "File Not Found"
        | isPermissionError e = return  $ HttpReply (reqversion q) 403 "Permission denied" [("server", "haskttpd")] "Permission denied"
        | isAlreadyInUseError e = return  $ HttpReply (reqversion q) 500 "Internal Error" [("server", "haskttpd")] "Already in use"
        | isIllegalOperation e = return  $ HttpReply (reqversion q) 500 "Internal Error" [("server", "haskttpd")] "Illegal Operation"
        | isUserError e = return  $ HttpReply (reqversion q) 500 "Internal Error" [("server", "haskttpd")] "User Error"
        | isEOFError e = return  $ HttpReply (reqversion q) 500 "Internal Error" [("server", "haskttpd")] "EOF Error"
        | otherwise = do
--      putStrLn (show e)
      return $ HttpReply (reqversion q) 500 "Internal Error" [("server", "haskttpd")] "An internal error occurred"

    generateResponseFile :: HttpRequest -> String -> ReaderT Config IO HttpReply
    generateResponseFile q file = do
      fileh <- liftIO $ openBinaryFile file ReadMode
      s <- liftIO $ hGetContents fileh
      return $ HttpReply (reqversion q) 200 "OK" [("server", "haskttpd")] s


    generateResponseIndex :: HttpRequest -> String -> ReaderT Config IO HttpReply
    generateResponseIndex q file = generateResponseFile q $ file ++ "/index.html"

    attemptAllResponseGenerators :: HttpRequest -> ReaderT Config IO HttpReply
    attemptAllResponseGenerators q = do
      conf <- ask
      let prependPath = getValueOrEmpty conf "DocumentRoot"
      let file = prependPath ++ reqressource q
          
      isDir <- liftIO $ Directory.doesDirectoryExist file
      if isDir
       then do
         generateResponseIndex q file
--      generateResponseAutoIndex q
       else do
         generateResponseFile q file
--      return $ HttpReply (reqversion q) 504 "Not Found" [("server", "haskttpd")] "File Not Found"

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
