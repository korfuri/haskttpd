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
        getValueOrEmpty :: Config -> String -> c
        getValue :: Config -> String -> Maybe c

      instance Configurable String where
          getValueOrEmpty c k = case lookup k (strValues c) of
                                  Nothing -> ""
                                  Just x -> x
                                  
          getValue c k = lookup k (strValues c)
          
      instance Configurable Int where
          getValueOrEmpty c k = case lookup k (numValues c) of
                                  Nothing -> 0
                                  Just x -> x
                                  
          getValue c k = lookup k (numValues c)

          

      getKey :: (Configurable c) => String -> ReaderT Config IO c
      getKey k = do
        conf <- ask
        return $ getValueOrEmpty conf k

