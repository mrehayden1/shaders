module Graphics.Shaders.Internal.Expr (
  S(..),
  V,
  F,

  ExprM,

  execExprM,

  tellStatement,

  toV4S
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.IO as HT
import Linear
import System.Mem.StableName

import Control.Monad.State.Extra


type MemoMap m a = HT.BasicHashTable (StableName (m a)) a

newtype MemoT v m a = MemoT {
  unMemoT :: ReaderT (MemoMap (MemoT v m) v) m a
} deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (MemoT v) where
  lift = MemoT . lift

runMemoT :: MonadIO m => MemoT v m a -> m a
runMemoT m = do
  t <- liftIO HT.new
  flip runReaderT t . unMemoT $ m

-- Shader expression of type `a` usable in shaders of type `x`.
newtype S x a = S { unS :: ExprM ByteString }

-- Phantom type representing vertex shaders
data V

-- Phantom type representing fragment shaders
data F

type ExprM a =
  MemoT ByteString
    (ReaderT Indent
      (StateT TempName
        (WriterT ByteString IO)))
  a

type Indent = Int
type TempName = Int

execExprM :: Int -> ExprM a -> IO ByteString
execExprM n = execWriterT . flip evalStateT 0 . flip runReaderT n . runMemoT

memoize :: MonadIO m => MemoT a m a -> MemoT a m a -> MemoT a m a
memoize key expr = do
  i <- liftIO . makeStableName $! key
  t <- MemoT ask
  ma <- liftIO . HT.lookup t $ i
  case ma of
    Just a  -> return a
    Nothing -> do
      a <- expr
      liftIO . HT.insert t i $ a
      return a

tellStatement :: ByteString -> ExprM ()
tellStatement s = do
  pad <- lift . asks $ BS.pack . flip replicate ' '
  lift . tell $ pad <> s <> ";\n"

-- Assign an expression to a variable name to avoid re-evaluation.
--
-- Note: any expression that uses tellAssigment may need to be INLINEd so that
-- the key passed to `memoize` refers to the same object on the heap.
tellAssignment :: GlType -> ExprM ByteString -> ExprM ByteString
tellAssignment typ expr = memoize expr $ do
  let typ' = glType typ
  n <- fmap (("t" <>) . BS.pack . show) . lift $ getNext
  v <- expr
  tellStatement $ typ' <> " " <> n <> " = " <> v
  return n

data GlType = GlFloat | GlVec4

glType :: GlType -> ByteString
glType GlFloat = "float"
glType GlVec4  = "vec4"

{-# INLINE bin #-}
bin :: GlType -> ByteString -> S x a -> S x a -> S x a
bin typ op a b = S . tellAssignment typ $ do
  a' <- unS a
  b' <- unS b
  return $ "(" <> a' <> " " <> op <> " " <> b' <> ")"

{-# INLINE fun1 #-}
fun1 :: GlType -> ByteString -> S x a -> S x a
fun1 typ fn a = S . tellAssignment typ $ do
  a' <- unS a
  return $ fn <> "(" <> a' <> ")"

{-# INLINE fun2 #-}
fun2 :: GlType -> ByteString -> S x a -> S x a -> S x a
fun2 typ fn a b = S . tellAssignment typ $ do
  a' <- unS a
  b' <- unS b
  return $ fn <> "(" <> a' <> ", " <> b' <> ")"

{-# INLINE pre #-}
pre :: GlType -> ByteString -> S x a -> S x a
pre typ op a = S . tellAssignment typ $ do
  a' <- unS a
  return $ "(" <> op <> a' <> ")"

instance Num (S x Float) where
  {-# INLINE (+) #-}
  (+) = bin GlFloat "+"
  {-# INLINE (-) #-}
  (-) = bin GlFloat "-"
  {-# INLINE (*) #-}
  (*) = bin GlFloat "*"
  {-# INLINE abs #-}
  abs = fun1 GlFloat "abs"
  {-# INLINE signum #-}
  signum = fun1 GlFloat "sign"
  {-# INLINE negate #-}
  negate = pre GlFloat "-"

  fromInteger n = S $ do
    return . BS.pack . show $ n

instance Fractional (S x Float) where
  {-# INLINE (/) #-}
  (/) = bin GlFloat "/"
  fromRational = S . return . ("float(" <>) . (<> ")") . BS.pack . show
    . (`asTypeOf` (undefined :: Float)) . fromRational

instance Floating (S x Float) where
  pi    = S $ return . BS.pack . show $ (pi :: Float)
  {-# INLINE sqrt #-}
  sqrt  = fun1 GlFloat "sqrt"
  {-# INLINE exp #-}
  exp   = fun1 GlFloat "exp"
  {-# INLINE log #-}
  log   = fun1 GlFloat "log"
  {-# INLINE (**) #-}
  (**)  = fun2 GlFloat "pow"
  {-# INLINE sin #-}
  sin   = fun1 GlFloat "sin"
  {-# INLINE cos #-}
  cos   = fun1 GlFloat "cos"
  {-# INLINE tan #-}
  tan   = fun1 GlFloat "tan"
  {-# INLINE asin #-}
  asin  = fun1 GlFloat "asin"
  {-# INLINE acos #-}
  acos  = fun1 GlFloat "acos"
  {-# INLINE atan #-}
  atan  = fun1 GlFloat "atan"
  {-# INLINE sinh #-}
  sinh  = fun1 GlFloat "sinh"
  {-# INLINE cosh #-}
  cosh  = fun1 GlFloat "cosh"
  {-# INLINE asinh #-}
  asinh = fun1 GlFloat "asinh"
  {-# INLINE atanh #-}
  atanh = fun1 GlFloat "atanh"
  {-# INLINE acosh #-}
  acosh = fun1 GlFloat "acosh"

-- Unsafe conversion to V4 (S x a)
toV4S :: ExprM ByteString -> V4 (S x a)
toV4S expr =
  let m = tellAssignment GlVec4 expr
  in V4
    (S $ fmap (<> ".x") m)
    (S $ fmap (<> ".y") m)
    (S $ fmap (<> ".z") m)
    (S $ fmap (<> ".w") m)
