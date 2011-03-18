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
    
    
    listenAt :: Int -> (Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> IO ()) -> IO ()
    listenAt port f = do
      let port' = toEnum port
      lsock <- socket AF_INET Stream 0
      setSocketOption lsock ReuseAddr 1
      bindSocket lsock $ SockAddrInet port' iNADDR_ANY
      listen lsock sOMAXCONN
      loop lsock 0 `finally` sClose lsock
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
      liftIO $ listenAt port $ (serveARequest currentConfig)
      return ()
