module Haskttpd.Config (Config(Config),
                        getValueOrEmpty,
                        getValue,
                        getKey)
    where
      import Control.Monad.Reader

      data Config = Config [(String, String)]
                    deriving (Eq, Show, Read)

      getValueOrEmpty :: Config -> String -> String
      getValueOrEmpty (Config c) k = case lookup k c of
                              Nothing -> ""
                              Just x -> x

      getValue k (Config c) = lookup c k

      getKey :: String -> ReaderT Config IO String
      getKey k = do
        conf <- ask
        return $ getValueOrEmpty conf k

