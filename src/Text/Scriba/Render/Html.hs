{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Render.Html
  ( Render(..)
  , RenderM(..)
  , RenderState(..)
  , initialRenderState
  , runRender
  , bumpHeaderDepth
  , renderMaybe
  , (??)
  , foldBy
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.State            ( MonadState(..)
                                                , State
                                                , runState
                                                , modify
                                                , gets
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           Text.Blaze.Html5               ( Html
                                                , (!)
                                                )
import qualified Text.Blaze.Internal           as BI
import qualified Text.Blaze.Html5              as H

{- TODO:

- multi-page rendering of the doc?

- standalone page rendering?

- pass along math macros?

- do I need to look at data-* or item-*?

- conditional rendering of empty things!

- Is the simple h1..h6 wrapping good enough?

-}

class Render a where
  render :: a -> RenderM Html

instance Render a => Render [a] where
  render = foldBy render

instance Render Void where
  render = absurd

-- Section titles run from h1 to h6, then top out there.
newtype RenderState = RenderState
  { rsHeaderDepth :: Int
  }

foldBy :: (Foldable t, Monoid c) => (a -> c) -> t a -> c
foldBy f = foldl' go mempty where go c a = c <> f a

renderMaybe :: Maybe Html -> (Html -> Html) -> Html
renderMaybe mh f = maybe mempty f mh

(??) :: BI.Attributable h => h -> Maybe H.Attribute -> h
(??) h Nothing  = h
(??) h (Just v) = h ! v

-- TODO: I _think_ the header depth should start at 1? Might need to
-- be configurable later. I think for standalone it should be 1, since
-- the document itself hopefully has a title (that we should
-- eventually gather and render).
initialRenderState :: RenderState
initialRenderState = RenderState { rsHeaderDepth = 1 }

newtype RenderM a = Render
  { unRender :: State RenderState a
  } deriving (Functor, Applicative, Monad, MonadState RenderState)

runRender :: RenderM a -> RenderState -> (a, RenderState)
runRender = runState . unRender

instance Semigroup a => Semigroup (RenderM a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (RenderM a) where
  mempty = pure mempty

bumpHeaderDepth :: RenderM a -> RenderM a
bumpHeaderDepth act = do
  n <- gets rsHeaderDepth
  setDepth $ n + 1
  a <- act
  setDepth n
  pure a
  where setDepth n = modify $ \s -> s { rsHeaderDepth = n }
