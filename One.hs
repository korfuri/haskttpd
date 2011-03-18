module One (
            One,
            success,
            runOne
           ) where

    import Control.Monad
    
    data One t a = One t
                 | Other a
                   deriving (Eq, Read, Show, Ord)
    
    bindOne :: One t a -> (a -> One t b) -> One t b
    bindOne (One x) _ = One x
    bindOne (Other a) f = f a

    success x = One x

    instance Monad (One t) where
      return x = Other x
      a >>= b = bindOne a b

    runOne :: One t a -> (a -> t) -> t
    runOne (One v) _ = v
    runOne (Other v) save = save v
