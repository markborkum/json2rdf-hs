{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.Described
( Described(..)
) where

class Described t a where
	describeWith :: (t -> t -> t) -> t -> a -> t

instance Described t () where
	describeWith _ z =
		const z

instance (Described t a) => Described t [a] where
	describeWith f z =
		foldr f z . fmap (describeWith f z)

instance (Described t a) => Described t (Maybe a) where
	describeWith f z (Just x) =
		describeWith f z x
	describeWith _ z _ =
		z

instance (Described t a, Described t b) => Described t (Either a b) where
	describeWith f z (Left x) =
		describeWith f z x
	describeWith f z (Right x) =
		describeWith f z x

instance (Described t a, Described t b) => Described t (a, b) where
	describeWith f z (a, b) =
		describeWith f z a `f` describeWith f z b

instance (Described t a, Described t b, Described t c) => Described t (a, b, c) where
	describeWith f z (a, b, c) =
		describeWith f z a `f` describeWith f z b `f` describeWith f z c

instance (Described t a, Described t b, Described t c, Described t d) => Described t (a, b, c, d) where
	describeWith f z (a, b, c, d) =
		describeWith f z a `f` describeWith f z b `f` describeWith f z c `f` describeWith f z d

instance (Described t a, Described t b, Described t c, Described t d, Described t e) => Described t (a, b, c, d, e) where
	describeWith f z (a, b, c, d, e) =
		describeWith f z a `f` describeWith f z b `f` describeWith f z c `f` describeWith f z d `f` describeWith f z e

instance (Described t a, Described t b, Described t c, Described t d, Described t e, Described t f) => Described t (a, b, c, d, e, f) where
	describeWith f z (a, b, c, d, e, f') =
		describeWith f z a `f` describeWith f z b `f` describeWith f z c `f` describeWith f z d `f` describeWith f z e `f` describeWith f z f'

instance (Described t a, Described t b, Described t c, Described t d, Described t e, Described t f, Described t g) => Described t (a, b, c, d, e, f, g) where
	describeWith f z (a, b, c, d, e, f', g) =
		describeWith f z a `f` describeWith f z b `f` describeWith f z c `f` describeWith f z d `f` describeWith f z e `f` describeWith f z f' `f` describeWith f z g
