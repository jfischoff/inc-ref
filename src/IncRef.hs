{-# LANGUAGE RecordWildCards #-}
module IncRef where
import Control.Concurrent.STM
import Control.Applicative

data IncRef a = IncRef
  { cache  :: STM a
  , stream :: STM a
  , dup    :: STM (IncRef a)
  }
  
data AnyF a b 
  = F (a -> b)
  | X a

instance Functor IncRef where
  fmap f IncRef {..} = IncRef (f <$> cache) (f <$> stream) (fmap f <$> dup)

instance Applicative IncRef where
  pure x = IncRef retry (return x) (return (pure x))  
  f <*> x 
     =  IncRef
     (cache f <*> cache x)
     (do 
          e <- (Left . F <$> stream f) <|> (Right . X <$> stream x)
          case e of
            Left  (F f') -> f'     <$> cache x
            Right (X x') -> ($ x') <$> cache f
      )
    $ (do 
         f' <- dup f
         x' <- dup x
         return $ f' <*> x'
      )

waitBoth :: IncRef a -> IncRef b -> STM (a, b)
waitBoth x y = stream $ (,) <$> x <*> y

mkIncRefFromTChan :: a -> TChan a -> STM (IncRef a)
mkIncRefFromTChan x chan = do
  var <- newTVar  x
  writeTVar var x
  let stream  = readTChan chan
      cache   = readTVar var

      dup = do 
        stream' <- dupTChan chan
        x'      <- cache
        mkIncRefFromTChan x' stream' 
  
  return $ IncRef  {..}

mkEmptyIncRefFromTChan :: TChan a -> STM (IncRef a)
mkEmptyIncRefFromTChan chan = do
  var <- newEmptyTMVar
  let stream  = readTChan chan
      cache   = readTMVar var

      dup = do 
        stream' <- dupTChan chan
        x'      <- cache
        mkEmptyIncRefFromTChan stream' 

  return $ IncRef  {..}
