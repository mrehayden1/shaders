module Data.Linear (
  V2(..),
  V3(..),
  V4(..),

  M33(..),
  M44(..),

  R1(..),
  R2(..),
  R3(..),
  R4(..)
) where

import Foreign.Ptr
import Foreign.Storable

data V2 a = V2 a a

instance Storable a => Storable (V2 a) where
  alignment _ = alignment (undefined :: a)
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
  alignment _ = alignment (undefined :: a)
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
  alignment _ = alignment (undefined :: a)
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


-- Matrices
--
-- All matrices are column major order representation

data M33 a = M33 (V3 a) (V3 a) (V3 a)

instance Storable a => Storable (M33 a) where
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    let ptr' = castPtr ptr
    a <- peek ptr'
    b <- peekElemOff ptr' 1
    c <- peekElemOff ptr' 2
    return $ M33 a b c
  poke ptr (M33 a b c) = do
    let ptr' = castPtr ptr
    poke ptr' a
    pokeElemOff ptr' 1 b
    pokeElemOff ptr' 2 c
  sizeOf _ = 3 * 3 * sizeOf (undefined :: a)

data M44 a = M44 (V4 a) (V4 a) (V4 a) (V4 a)

instance Storable a => Storable (M44 a) where
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    let ptr' = castPtr ptr
    a <- peek ptr'
    b <- peekElemOff ptr' 1
    c <- peekElemOff ptr' 2
    d <- peekElemOff ptr' 3
    return $ M44 a b c d
  poke ptr (M44 a b c d) = do
    let ptr' = castPtr ptr
    poke ptr' a
    pokeElemOff ptr' 1 b
    pokeElemOff ptr' 2 c
    pokeElemOff ptr' 3 d
  sizeOf _ = 4 * 4 * sizeOf (undefined :: a)

-- Basis vector accesors

class R1 f a where
  _x :: f -> a
  _x = _0
  _r :: f -> a
  _r = _x
  _u :: f -> a
  _u = _x
  _0 :: f -> a
  _0 = _x

instance R1 (V2 a) a where
  _x (V2 x _) = x

instance R1 (V3 a) a where
  _x (V3 x _ _) = x

instance R1 (V4 a) a where
  _x (V4 x _ _ _) = x

instance R1 (M33 a) (V3 a) where
  _0 (M33 x _ _) = x

instance R1 (M44 a) (V4 a) where
  _0 (M44 x _ _ _) = x


class R1 f a => R2 f a where
  _y :: f -> a
  _y = _1
  _v :: f -> a
  _v = _y
  _g :: f -> a
  _g = _y
  _1 :: f -> a
  _1 = _y

instance R2 (V2 a) a where
  _y (V2 _ y) = y

instance R2 (V3 a) a where
  _y (V3 _ y _) = y

instance R2 (V4 a) a where
  _y (V4 _ y _ _) = y

instance R2 (M33 a) (V3 a) where
  _1 (M33 _ y _) = y

instance R2 (M44 a) (V4 a) where
  _1 (M44 _ y _ _) = y


class R2 f a => R3 f a where
  _z :: f -> a
  _z = _2
  _b :: f -> a
  _b = _z
  _2 :: f -> a
  _2 = _z

instance R3 (V3 a) a where
  _z (V3 _ _ z) = z

instance R3 (V4 a) a where
  _z (V4 _ _ z _) = z

instance R3 (M33 a) (V3 a) where
  _2 (M33 _ _ z) = z

instance R3 (M44 a) (V4 a) where
  _2 (M44 _ _ z _) = z


class R3 f a => R4 f a where
  _w :: f -> a
  _w = _3
  _a :: f -> a
  _a = _w
  _3 :: f -> a
  _3 = _w

instance R4 (V4 a) a where
  _w (V4 _ _ _ w) = w

instance R4 (M44 a) (V4 a) where
  _3 (M44 _ _ _ w) = w
