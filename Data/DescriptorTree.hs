{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DescriptorTree
( DescriptorTree(..)
, fromList , toList , toList'
, union , unions
, intersection , intersections
, UnionDescriptorTree(..) , IntersectionDescriptorTree(..)
, pp_DescriptorTree
) where

import Prelude hiding (foldr)

import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.Monad (ap, liftM2)
import Data.Canonical (Canonical(canonicalize))
import Data.Foldable (Foldable(foldMap, foldr))
import Data.Function (on)
import Data.Monoid (Monoid(mempty, mappend, mconcat))
import Data.Traversable (Traversable(traverse), fmapDefault, foldMapDefault)
import Text.PrettyPrint.HughesPJ (Doc, (<>), braces, brackets, char, colon, comma, hcat, hsep, int, parens, space, text, zeroWidthText)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data DescriptorTree k v = BottomNode
                        | ScalarNode v
                        | HomListNode (DescriptorTree k v)
                        | HetListNode [(Int, DescriptorTree k v)]
                        | AscListNode [(k, DescriptorTree k v)]
                        | UnionNode [DescriptorTree k v]
                        | TopNode
                          deriving (Eq, Ord, Read, Show)

fromList :: [DescriptorTree k v] -> DescriptorTree k v
fromList [] =
  BottomNode
fromList [t] =
  t
fromList ts =
  UnionNode ts

toList :: DescriptorTree k v -> [DescriptorTree k v]
toList BottomNode =
  []
toList (UnionNode ts) =
  ts
toList t =
  [t]

toList' :: DescriptorTree k v -> [DescriptorTree k v] -> [DescriptorTree k v]
toList' BottomNode =
  id
toList' (UnionNode []) =
  id
toList' (UnionNode [x]) =
  (x :)
toList' (UnionNode xs) =
  (xs ++)
toList' x =
  (x :)

instance (Ord k, Ord v) => Canonical (DescriptorTree k v) where
  canonicalize (HomListNode t) =
    HomListNode (canonicalize t)
  canonicalize (HetListNode ts) =
    HetListNode ((fmap . fmap) canonicalize ts)
  canonicalize (AscListNode ts) =
    AscListNode ((fmap . fmap) canonicalize ts)
  canonicalize (UnionNode ts) =
    fromList . S.toList . S.map canonicalize . S.fromList . foldr toList' [] $ ts
  canonicalize t =
    t

instance Bounded (DescriptorTree k v) where
  minBound =
    BottomNode
  maxBound =
    TopNode

instance Functor (DescriptorTree k) where
  fmap =
    fmapDefault

instance Foldable (DescriptorTree k) where
  foldMap =
    foldMapDefault

instance Traversable (DescriptorTree k) where
  traverse _ BottomNode =
    pure BottomNode
  traverse f (ScalarNode x) =
    ScalarNode <$> f x
  traverse f (HomListNode t) =
    HomListNode <$> traverse f t
  traverse f (HetListNode ts) =
    HetListNode <$> ((traverse . (\g (a, b) -> (,) a <$> g b) . traverse) f ts)
  traverse f (AscListNode ts) =
    AscListNode <$> ((traverse . (\g (a, b) -> (,) a <$> g b) . traverse) f ts)
  traverse f (UnionNode ts) =
    UnionNode <$> ((traverse . traverse) f ts)
  traverse _ TopNode =
    pure TopNode

instance Applicative (DescriptorTree k) where
  pure =
    return
  (<*>) =
    ap

instance Monad (DescriptorTree k) where
  return =
    ScalarNode
  BottomNode >>= _ =
    BottomNode
  (ScalarNode x) >>= f =
    f x
  (HomListNode t) >>= f =
    HomListNode (t >>= f)
  (HetListNode ts) >>= f =
    HetListNode ((fmap . fmap) (>>= f) ts)
  (AscListNode ts) >>= f =
    AscListNode ((fmap . fmap) (>>= f) ts)
  (UnionNode ts) >>= f =
    UnionNode (fmap (>>= f) ts)
  TopNode >>= _ =
    TopNode

union :: (Ord k, Ord v) => DescriptorTree k v -> DescriptorTree k v -> DescriptorTree k v
union =
  on go canonicalize
    where
      go t BottomNode =
        t
      go BottomNode t =
        t
      go _ TopNode =
        TopNode
      go TopNode _ =
        TopNode
      go (HomListNode t1) (HomListNode t2) =
        HomListNode (t1 `go` t2)
      go (HetListNode ts1) (HetListNode ts2) =
        HetListNode (M.toList (on (M.unionWith go) M.fromList ts1 ts2))
      go (AscListNode ts1) (AscListNode ts2) =
        AscListNode (M.toList (on (M.unionWith go) M.fromList ts1 ts2))
      go (UnionNode ts1) (UnionNode ts2) =
        fromList (liftM2 go ts1 ts2)
      go t1 t2 =
        case compare t1 t2 of
          EQ ->
            t1
          LT ->
            UnionNode [t1, t2]
          GT ->
            UnionNode [t2, t1]

unions :: (Ord k, Ord v, Foldable t) => t (DescriptorTree k v) -> DescriptorTree k v
unions =
  foldr union minBound

intersection :: (Ord k, Ord v) => DescriptorTree k v -> DescriptorTree k v -> DescriptorTree k v
intersection _ BottomNode =
  BottomNode
intersection BottomNode _ =
  BottomNode
intersection t TopNode =
  t
intersection TopNode t =
  t
intersection (HomListNode t1) (HomListNode t2) =
  HomListNode (t1 `intersection` t2)
intersection (HetListNode ts1) (HetListNode ts2) =
  HetListNode (M.toList (on (M.intersectionWith intersection) M.fromList ts1 ts2))
intersection (AscListNode ts1) (AscListNode ts2) =
  AscListNode (M.toList (on (M.intersectionWith intersection) M.fromList ts1 ts2))
intersection t1 t2 =
  case compare t1 t2 of
    EQ ->
      t1
    _ ->
      BottomNode

intersections :: (Ord k, Ord v, Foldable t) => t (DescriptorTree k v) -> DescriptorTree k v
intersections =
  foldr intersection maxBound

newtype UnionDescriptorTree k v = UnionDescriptorTree { unionDescriptorTree :: DescriptorTree k v }
  deriving (Bounded, Eq, Read, Show, Functor, Foldable, Traversable, Applicative, Monad)

newtype IntersectionDescriptorTree k v = IntersectionDescriptorTree { intersectionDescriptorTree :: DescriptorTree k v }
  deriving (Bounded, Eq, Read, Show, Functor, Foldable, Traversable, Applicative, Monad)

instance (Ord k, Ord v) => Monoid (UnionDescriptorTree k v) where
  mempty =
    minBound
  (UnionDescriptorTree t1) `mappend` (UnionDescriptorTree t2) =
    UnionDescriptorTree (t1 `union` t2)
  mconcat =
    UnionDescriptorTree . unions . fmap unionDescriptorTree

instance (Ord k, Ord v) => Monoid (IntersectionDescriptorTree k v) where
  mempty =
    maxBound
  (IntersectionDescriptorTree t1) `mappend` (IntersectionDescriptorTree t2) =
    IntersectionDescriptorTree (t1 `intersection` t2)
  mconcat =
    IntersectionDescriptorTree . intersections . fmap intersectionDescriptorTree

instance (Show k, Show v) => Pretty (DescriptorTree k v) where
  pPrint =
    pp_DescriptorTree True

pp_DescriptorTree :: (Show k, Show v) => Bool -> DescriptorTree k v -> Doc
pp_DescriptorTree b =
  pp (if b then withWhitespaceStyle else withoutWhitespaceStyle) 0
    where
      pp _ _ BottomNode =
        char '┴'
      pp _ _ (ScalarNode v) =
        text (show v)
      pp s n (HomListNode t) =
        brackets . _brackets s n . _brackets_elem s n . pp s (succ n) $ t
      pp s n (HetListNode ts) =
        brackets . _brackets s n . hcat . L.intersperse (_comma s comma) . fmap (_brackets_elem s n . mkDoc) $ ts
          where
            mkDoc (idx, t) =
              int idx <> _colon s colon <> pp s (succ n) t
      pp s n (AscListNode ts) =
        braces . _braces s n . hcat . L.intersperse (_comma s comma) . fmap (_braces_elem s n . mkDoc) $ ts
          where
            mkDoc (key, t) =
              text (show key) <> _colon s colon <> pp s (succ n) t
      pp s n (UnionNode ts) =
        parens . _parens s n . hcat . L.intersperse (_parens_binop s $ char '∪') . fmap (_parens_elem s n . pp s n) $ ts
      pp _ _ TopNode =
        char '┬'

data DescriptorTreeStyle = DescriptorTreeStyle { _braces        :: Int -> Doc -> Doc
                                               , _braces_elem   :: Int -> Doc -> Doc
                                               , _brackets      :: Int -> Doc -> Doc
                                               , _brackets_elem :: Int -> Doc -> Doc
                                               , _colon         :: Doc -> Doc
                                               , _comma         :: Doc -> Doc
                                               , _parens        :: Int -> Doc -> Doc
                                               , _parens_elem   :: Int -> Doc -> Doc
                                               , _parens_binop  :: Doc -> Doc
                                               }

withoutWhitespaceStyle :: DescriptorTreeStyle
withoutWhitespaceStyle =
  DescriptorTreeStyle { _braces        = const id
                      , _braces_elem   = const id
                      , _brackets      = const id
                      , _brackets_elem = const id
                      , _colon         = id
                      , _comma         = id
                      , _parens        = const id
                      , _parens_elem   = const id
                      , _parens_binop  = id
                      }

withWhitespaceStyle :: DescriptorTreeStyle
withWhitespaceStyle =
  DescriptorTreeStyle { _braces        = wrapline
                      , _braces_elem   = indent'
                      , _brackets      = wrapline
                      , _brackets_elem = indent'
                      , _colon         = spacespace
                      , _comma         = (<> newline)
                      , _parens        = const spacespace
                      , _parens_elem   = const id
                      , _parens_binop  = spacespace
                      }
    where
      indent :: Int -> Doc
      indent =
        hcat . flip replicate (space <> space)
      indent' :: Int -> Doc -> Doc
      indent' n =
        (indent (succ n) <>)
      newline :: Doc
      newline =
        zeroWidthText "\n"
      spacespace :: Doc -> Doc
      spacespace =
        (<> space) . (space <>)
      wrapline :: Int -> Doc -> Doc
      wrapline n doc =
        newline <> doc <> newline <> indent n
