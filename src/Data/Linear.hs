module Data.Linear (
  V2(..),
  V3(..),
  V4(..),

  R1(..),
  R2(..),
  R3(..),
  R4(..)
) where

import Foreign.Ptr
import Foreign.Storable

data V2 a = V2 a a

instance Storable a => Storable (V2 a) where
  alignment _ = 4
  peek ptr = do
    a <- peek (castPtr ptr)
    b <- peekByteOff ptr (sizeOf a)
    return $ V2 a b
  poke ptr (V2 a b) = do
    poke (castPtr ptr) a
    pokeByteOff ptr (sizeOf a) b
  sizeOf _ = sizeOf (undefined :: a) * 2

data V3 a = V3 a a a

instance Storable a => Storable (V3 a) where
  alignment _ = 4
  peek ptr = do
    let ptr' = castPtr ptr
    a <- peek ptr'
    b <- peekElemOff ptr' 1
    c <- peekElemOff ptr' 2
    return $ V3 a b c
  poke ptr (V3 a b c) = do
    let ptr' = castPtr ptr
    poke ptr' a
    pokeElemOff ptr' 1 b
    pokeElemOff ptr' 2 c
  sizeOf _ = 3 * sizeOf (undefined :: a)

data V4 a = V4 a a a a

instance Storable a => Storable (V4 a) where
  alignment _ = 4
  peek ptr = do
    let ptr' = castPtr ptr
    a <- peek ptr'
    b <- peekElemOff ptr' 1
    c <- peekElemOff ptr' 2
    d <- peekElemOff ptr' 3
    return $ V4 a b c d
  poke ptr (V4 a b c d) = do
    let ptr' = castPtr ptr
    poke ptr' a
    pokeElemOff ptr' 1 b
    pokeElemOff ptr' 2 c
    pokeElemOff ptr' 3 d
  sizeOf _ = 3 * sizeOf (undefined :: a)


class R1 f a where
  x :: f -> a

instance R1 (V2 a) a where
  x (V2 x' _) = x'

instance R1 (V3 a) a where
  x (V3 x' _ _) = x'

instance R1 (V4 a) a where
  x (V4 x' _ _ _) = x'


class R1 f a => R2 f a where
  y :: f -> a

instance R2 (V2 a) a where
  y (V2 _ a) = a

instance R2 (V3 a) a where
  y (V3 _ a _) = a

instance R2 (V4 a) a where
  y (V4 _ a _ _) = a


class R2 f a => R3 f a where
  z :: f -> a

instance R3 (V3 a) a where
  z (V3 _ _ a) = a

instance R3 (V4 a) a where
  z (V4 _ _ a _) = a


class R3 f a => R4 f a where
  w :: f -> a

instance R4 (V4 a) a where
  w (V4 _ _ _ a) = a
