{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Decorate where

-- Decorate a value using a monadic action
class Monad m => Decorate m a where
  decorate :: a -> m a
