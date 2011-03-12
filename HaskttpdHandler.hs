module Haskttpd.Handler (
                         handle
                        ) where

    import System.IO
    import qualified Network.Socket.Internal as NSI
    import qualified Control.Exception as C (catch)
    import Control.Monad.Reader
    import Haskttpd.Config
    import Haskttpd.Parser

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
