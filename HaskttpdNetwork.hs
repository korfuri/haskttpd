module Haskttpd.Network (
                        startServer
                       ) where
    import Network.Socket
    import Network.BSD
    import qualified Network.Socket.Internal as NSI
    import System.IO (Handle, IOMode(ReadWriteMode), hPutStrLn, hGetLine, hClose)
    import Control.Concurrent (forkIO)
    import Control.Exception (finally)
    import Haskttpd.Config
    import Haskttpd.Handler
    import Control.Monad.Reader
    
    
    listenAt :: Int -> (Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> ReaderT Config IO ()) -> ReaderT Config IO ()
    listenAt port f = do
      lsock <- liftIO $ do
        let port' = toEnum port
        lsock <- socket AF_INET Stream 0
        setSocketOption lsock ReuseAddr 1
        bindSocket lsock $ SockAddrInet port' iNADDR_ANY
        listen lsock sOMAXCONN
        return lsock
      return ((runReaderT ask $ loop lsock 0) `finally` sClose lsock)
      return ()
          where
            loop lsock cid = do
                        (sock, SockAddrInet portfrom addrfrom) <- liftIO $ accept lsock
                        handle <- liftIO $ socketToHandle sock ReadWriteMode
                        f handle cid (addrfrom, portfrom)
                        loop lsock $ cid + 1

    serveARequest :: Config -> Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> IO ()
    serveARequest currentConfig h cid (addr, port) = do
      forkIO (((runReaderT $ handle h cid (addr, port)) currentConfig) `finally` hClose h)
      return ()

    startServer :: ReaderT Config IO ()
    startServer = do
      currentConfig <- ask
      port <- (getKey "BindPort")
      let port' = (read port) :: Int
      listenAt 12345 $ \h cid (addr, port) -> do
        liftIO $ putStrLn "a"
        liftIO $ serveARequest currentConfig h cid (addr, port)
      return ()
