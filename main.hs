import Network.Socket
import Network.BSD
import qualified Network.Socket.Internal as NSI
import System.IO (Handle, IOMode(ReadWriteMode), hPutStrLn, hGetLine, hClose)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Httphandler

listenAt :: Int -> (Handle -> Int -> (NSI.HostAddress, NSI.PortNumber) -> IO ()) -> IO ()
listenAt port f = do
  let port' = toEnum port
  lsock <- socket AF_INET Stream 0
  setSocketOption lsock ReuseAddr 1
  bindSocket lsock $ SockAddrInet port' iNADDR_ANY
  listen lsock sOMAXCONN
  loop lsock 0 `finally` sClose lsock
    where
      loop lsock cid = do
        (sock, SockAddrInet portfrom addrfrom) <- accept lsock
        handle <- socketToHandle sock ReadWriteMode
        f handle cid (addrfrom, portfrom)
        loop lsock $ cid + 1

main = withSocketsDo $ do
         listenAt 12345 serveHTTP
    where
      serveHTTP h cid (addr, port) = do
          forkIO $ (do handle h cid (addr, port) `finally` hClose h)
          return ()
