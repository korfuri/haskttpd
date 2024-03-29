import Control.Monad.Reader
import Haskttpd.Network
import Haskttpd.Config
import Network.Socket

main = withSocketsDo $ do
         runReaderT startServer $ Config
                        [
                         ("DocumentRoot", "/var/www/"),
                         ("BindAddr", "127.0.0.1")
                        ] [
                         ("BindPort", 8080)
                        ]
