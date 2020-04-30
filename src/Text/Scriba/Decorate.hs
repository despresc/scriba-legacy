{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Decorate where

-- Decorate a value using a monadic action. The exact details will
-- change depending on the monad being used.

-- TODO: make this generic derivable? Or maybe this should become a
-- higher-order helper class that can be derived generically, so we
-- can, say, easily number the content of things that themselves don't
-- participate in numbering.
class Monad m => Decorate m a where
  decorate :: a -> m a
