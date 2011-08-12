{-# LANGUAGE TypeSynonymInstances #-}

module Haskttpd.Config (Config(Config),
                        getValueOrEmpty,
                        getValue,
                        getKey)
    where
      import Control.Monad.Reader

      data Config = Config {
            strValues :: [(String, String)],
            numValues :: [(String, Int)]
          } deriving (Eq, Show, Read)

      class Configurable c where
        getValue :: Config -> String -> Maybe c
        empty :: c
        getValueOrEmpty :: Config -> String -> c
        getValueOrEmpty c k = 
          case v of
            Just x -> x
            Nothing -> empty
          where v = getValue c k

      instance Configurable String where
          getValue c k = lookup k (strValues c)
          empty = ""
          
      instance Configurable Int where
          getValue c k = lookup k (numValues c)
          empty = 0
          
      getKey :: (Configurable c) => String -> ReaderT Config IO c
      getKey k = do
        conf <- ask
        return $ getValueOrEmpty conf k

