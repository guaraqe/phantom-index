{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Indexed
  ( HasIndexed (..)
  , mvIx
  ) where

import Control.ConstraintClasses
import Control.Newtype

import Data.Bifunctor

import GHC.Exts

--------------------------------------------------------------------------------

class HasIndexed f where
  data Indexed f :: * -> * -> *
  unIx :: (Indexed f) i a -> f a
  mkIx :: f a -> (Indexed f) i a

mvIx :: HasIndexed f => (Indexed f) i a -> (Indexed f) j a
mvIx = mkIx . unIx

instance HasIndexed [] where
  newtype Indexed [] i a = List [a]
  unIx (List l) = l
  mkIx l = List l

--------------------------------------------------------------------------------

type instance Dom (Indexed f i) = Dom f

--------------------------------------------------------------------------------

instance (Dom f a, HasIndexed f, IsList (f a)) => IsList ((Indexed f) i a) where
  type Item ((Indexed f) i a) = Item (f a)
  fromList = mkIx . fromList
  {-# INLINE fromList #-}
  toList = toList . unIx
  {-# INLINE toList #-}

--------------------------------------------------------------------------------

instance (HasIndexed f, Functor f) => Functor ((Indexed f) i) where
  fmap f = mkIx . fmap f . unIx
  {-# INLINE fmap #-}

instance (HasIndexed f, Foldable f) => Foldable ((Indexed f) i) where
  foldr f x = foldr f x . unIx
  {-# INLINE foldr #-}
  foldl f x = foldl f x . unIx
  {-# INLINE foldl #-}
  foldMap f = foldMap f . unIx
  {-# INLINE foldMap #-}
  length = length . unIx
  {-# INLINE length #-}

instance (HasIndexed f, Traversable f) => Traversable ((Indexed f) i) where
  traverse f = fmap mkIx . traverse f . unIx
  {-# INLINE traverse #-}
  sequenceA = fmap mkIx . sequenceA . unIx
  {-# INLINE sequenceA #-}
  mapM f = fmap mkIx . mapM f . unIx
  {-# INLINE mapM #-}
  sequence = fmap mkIx . sequence . unIx
  {-# INLINE sequence #-}

--------------------------------------------------------------------------------

instance (HasIndexed f, CFunctor f) => CFunctor ((Indexed f) i) where
  _fmap f = mkIx . _fmap f . unIx
  {-# INLINE _fmap #-}

instance (HasIndexed f, CFoldable f) => CFoldable ((Indexed f) i) where
  _foldr f x = _foldr f x . unIx
  {-# INLINE _foldr #-}
  _foldr' f x = _foldr' f x . unIx
  {-# INLINE _foldr' #-}
  _foldl f x = _foldl f x . unIx
  {-# INLINE _foldl #-}
  _foldl' f x = _foldl' f x . unIx
  {-# INLINE _foldl' #-}
  _fold = _fold . unIx
  {-# INLINE _fold #-}
  _foldMap f = _foldMap f . unIx
  {-# INLINE _foldMap #-}
  _toList = _toList . unIx
  {-# INLINE _toList #-}
  _length = _length . unIx
  {-# INLINE _length #-}
  _mapM_ f = _mapM_ f . unIx
  {-# INLINE _mapM_ #-}

instance (HasIndexed f, CFoldableFunctor f) => CFoldableFunctor ((Indexed f) i)

instance (HasIndexed f, CTraversable f) => CTraversable ((Indexed f) i) where
  _traverse f = fmap mkIx . _traverse f . unIx
  {-# INLINE _traverse #-}
  _sequenceA = fmap mkIx . _sequenceA . unIx
  {-# INLINE _sequenceA #-}
  _mapM f = fmap mkIx . _mapM f . unIx
  {-# INLINE _mapM #-}
  _sequence = fmap mkIx . _sequence . unIx
  {-# INLINE _sequence #-}

instance (HasIndexed f, CZip f) => CApply ((Indexed f) i) where
  _liftA2 = _zipWith
  _liftA3 = _zipWith3
  _liftA4 = _zipWith4

instance (HasIndexed f, CZip f) => CZip ((Indexed f) i) where
  --_zip = \v1 v2 ->
  --  mkIx (_zip (unIx v1) (unIx v2))
  --{-# INLINE _zip #-}
  _zipWith f = \v1 v2 ->
    mkIx (_zipWith f (unIx v1) (unIx v2))
  {-# INLINE _zipWith #-}
  _zipWith3 f = \v1 v2 v3 ->
    mkIx (_zipWith3 f (unIx v1) (unIx v2) (unIx v3))
  {-# INLINE _zipWith3 #-}
  _zipWith4 f = \v1 v2 v3 v4 ->
    mkIx (_zipWith4 f (unIx v1) (unIx v2) (unIx v3) (unIx v4))
  {-# INLINE _zipWith4 #-}
  --_zap = \v1 v2 ->
  --  mkIx (_zap (unIx v1) (unIx v2))
  --{-# INLINE _zap #-}

instance (HasIndexed f, CUnzip f) => CUnzip ((Indexed f) i) where
  _unzip = bimap mkIx mkIx . _unzip . unIx
  _unzipWith f = bimap mkIx mkIx . _unzipWith f . unIx
  {-# INLINE _unzipWith #-}

--------------------------------------------------------------------------------

type instance CKey ((Indexed f) i) = i

instance (HasIndexed f, CLookup f, Newtype i (CKey f)) => CLookup ((Indexed f) i) where
  _lookup i v = _lookup (unpack i) (unIx v)
  {-# INLINE [1] _lookup #-}

instance (HasIndexed f, CIndexable f, Newtype i (CKey f)) => CIndexable ((Indexed f) i) where
  _index v i = _index (unIx v) (unpack i)
  {-# INLINE [1] _index #-}

instance (HasIndexed f, CKeyFunctor f, Newtype i (CKey f)) => CKeyFunctor ((Indexed f) i) where
  _imap f = \v -> mkIx (_imap (f . pack) (unIx v))
  {-# INLINE _imap #-}

instance (HasIndexed f, CKeyZip f, Newtype i (CKey f)) => CKeyZip ((Indexed f) i) where
  _izipWith f = \v1 v2 ->
    mkIx (_izipWith (f . pack) (unIx v1) (unIx v2))
  {-# INLINE _izipWith #-}

instance (HasIndexed f, CKeyFoldable f, Newtype i (CKey f)) =>
  CKeyFoldable ((Indexed f) i) where

  _itoList = fmap (\(i,x) -> (pack i, x)) . _itoList . unIx
  {-# INLINE _itoList #-}
  _ifoldMap f = _ifoldMap (f . pack) . unIx
  {-# INLINE _ifoldMap #-}
  _ifoldr f x = _ifoldr (f . pack) x . unIx
  {-# INLINE _ifoldr #-}
  _ifoldr' f x = _ifoldr' (f . pack) x . unIx
  {-# INLINE _ifoldr' #-}
  _ifoldl f x = _ifoldl (flip (flip f . pack)) x . unIx
  {-# INLINE _ifoldl #-}
  _ifoldl' f x = _ifoldl' (flip (flip f . pack)) x . unIx
  {-# INLINE _ifoldl' #-}

instance (HasIndexed f, CAdjustable f, Newtype i (CKey f)) =>  CAdjustable ((Indexed f) i) where
  _update f v l = mkIx (_update f (unIx v) (fmap (\(n,x) -> (unpack n,x)) l))
  {-# INLINE _update #-}
  _adjust f i = \v -> mkIx (_adjust f (unpack i) (unIx v))
  {-# INLINE _adjust #-}
  _replace i x = \v -> mkIx (_replace (unpack i) x (unIx v))
  {-# INLINE _replace #-}

instance (HasIndexed f, CKeyFoldableFunctor f, Newtype i (CKey f)) => CKeyFoldableFunctor ((Indexed f) i)

instance (HasIndexed f, CKeyTraversable f, Newtype i (CKey f)) =>
  CKeyTraversable ((Indexed f) i) where
  _itraverse f = fmap mkIx . _itraverse (f . pack) . unIx
  {-# INLINE _itraverse #-}

instance (HasIndexed f, CEq1 f) => CEq1 ((Indexed f) i) where
  _liftEq eq x y = _liftEq eq (unIx x) (unIx y)
