module Graphics.Shaders.Internal.Expr (
  S(..),
  V,
  F,

  ExprM,

  execExprM,

  tellStatement,

  toV2S,
  toV2S',
  toV3S,
  toV3S',
  toV4S',
  toV4S
) where

import Control.Lens hiding (pre)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
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

data GlType =
    GlFloat
  | GlMat2x2 | GlMat2x3 | GlMat2x4
  | GlMat3x2 | GlMat3x3 | GlMat3x4
  | GlMat4x2 | GlMat4x3 | GlMat4x4
  | GlVec2 | GlVec3 | GlVec4

glType :: GlType -> ByteString
glType GlFloat  = "float"
glType GlMat2x2 = "mat2"
glType GlMat2x3 = "mat2x3"
glType GlMat2x4 = "mat2x4"
glType GlMat3x2 = "mat3x2"
glType GlMat3x3 = "mat3"
glType GlMat3x4 = "mat3x4"
glType GlMat4x2 = "mat4x2"
glType GlMat4x3 = "mat4x3"
glType GlMat4x4 = "mat4"
glType GlVec2   = "vec2"
glType GlVec3   = "vec3"
glType GlVec4   = "vec4"

{-# INLINE bin #-}
bin :: GlType -> ByteString -> S x a -> S x b -> S x c
bin typ op' a b = S . tellAssignment typ $ do
  a' <- unS a
  b' <- unS b
  return $ "(" <> a' <> " " <> op' <> " " <> b' <> ")"

{-# INLINE fun1 #-}
fun1 :: GlType -> ByteString -> S x a -> S x b
fun1 typ fn a = S . tellAssignment typ $ do
  a' <- unS a
  return $ fn <> "(" <> a' <> ")"

{-# INLINE fun2 #-}
fun2 :: GlType -> ByteString -> S x a -> S x b -> S x c
fun2 typ fn a b = S . tellAssignment typ $ do
  a' <- unS a
  b' <- unS b
  return $ fn <> "(" <> a' <> ", " <> b' <> ")"

{-# INLINE pre #-}
pre :: GlType -> ByteString -> S x a -> S x b
pre typ op' a = S . tellAssignment typ $ do
  a' <- unS a
  return $ "(" <> op' <> a' <> ")"

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

-- Conversion to V2 (S x a) with assignment
toV2S :: S x (V2 a) -> V2 (S x a)
toV2S = toV2S' . tellAssignment GlVec2 . unS

-- Unsafe conversion to V2 (S x a)
toV2S' :: ExprM ByteString -> V2 (S x a)
toV2S' expr =
  V2
    (S $ fmap (<> "[0]") expr)
    (S $ fmap (<> "[1]") expr)

-- Conversion to V3 (S x a) with assignment
toV3S :: S x (V3 a) -> V3 (S x a)
toV3S = toV3S' . tellAssignment GlVec3 . unS

-- Unsafe conversion to V3 (S x a)
toV3S' :: ExprM ByteString -> V3 (S x a)
toV3S' expr =
  V3
    (S $ fmap (<> "[0]") expr)
    (S $ fmap (<> "[1]") expr)
    (S $ fmap (<> "[2]") expr)

-- Conversion to V4 (S x a) with assignment
toV4S :: S x (V4 a) -> V4 (S x a)
toV4S = toV4S' . tellAssignment GlVec4 . unS

-- Unsafe conversion to V4 (S x a)
toV4S' :: ExprM ByteString -> V4 (S x a)
toV4S' expr =
  V4
    (S $ fmap (<> "[0]") expr)
    (S $ fmap (<> "[1]") expr)
    (S $ fmap (<> "[2]") expr)
    (S $ fmap (<> "[3]") expr)

fromV :: Foldable t => (a -> S x b) -> GlType -> t a -> S x c
fromV f s v = S $ do
  params <- mapM (unS . f) $ toList v
  return $ glType s <> "(" <> BS.intercalate ", " params <> ")"

-- Conversion of V2 (S x a) to S x (V2 a)
fromVec2 :: V2 (S x Float) -> S x (V2 Float)
fromVec2 = fromV id GlVec2

-- Conversion of V3 (S x a) to S x (V3 a)
fromVec3 :: V3 (S x Float) -> S x (V3 Float)
fromVec3 = fromV id GlVec3

-- Conversion of V4 (S x a) to S x (V4 a)
fromVec4 :: V4 (S x Float) -> S x (V4 Float)
fromVec4 = fromV id GlVec4

-- Conversion of matrices
fromMat22 :: V2 (V2 (S x Float)) -> S x (V2 (V2 Float))
fromMat22 = fromV fromVec2 GlMat2x2
fromMat23 :: V2 (V3 (S x Float)) -> S x (V2 (V3 Float))
fromMat23 = fromV fromVec3 GlMat2x3
fromMat24 :: V2 (V4 (S x Float)) -> S x (V2 (V4 Float))
fromMat24 = fromV fromVec4 GlMat2x4

fromMat32 :: V3 (V2 (S x Float)) -> S x (V3 (V2 Float))
fromMat32 = fromV fromVec2 GlMat3x2
fromMat33 :: V3 (V3 (S x Float)) -> S x (V3 (V3 Float))
fromMat33 = fromV fromVec3 GlMat3x3
fromMat34 :: V3 (V4 (S x Float)) -> S x (V3 (V4 Float))
fromMat34 = fromV fromVec4 GlMat3x4

fromMat42 :: V4 (V2 (S x Float)) -> S x (V4 (V2 Float))
fromMat42 = fromV fromVec2 GlMat4x2
fromMat43 :: V4 (V3 (S x Float)) -> S x (V4 (V3 Float))
fromMat43 = fromV fromVec3 GlMat4x3
fromMat44 :: V4 (V4 (S x Float)) -> S x (V4 (V4 Float))
fromMat44 = fromV fromVec4 GlMat4x4

-- Convenience functions
unV1 :: V1 a -> a
unV1 = (^. _x)

mulToV2 :: S x a -> S x b -> V2 (S x Float)
mulToV2 a = toV2S . bin GlVec2 "*" a

mulToV3 :: S x a -> S x b -> V3 (S x Float)
mulToV3 a = toV3S . bin GlVec3 "*" a

mulToV4 :: S x a -> S x b -> V4 (S x Float)
mulToV4 a = toV4S . bin GlVec4 "*" a


-- Rewrite rules
--
-- Since matrices are row major in our lib and column major in GLSL, we need
-- to reverse the arguments in matrix multiplication, so pre-multiplication
-- becomes post-multiplication.

--
-- Vector inner products
--
{-# INLINE dot_12_21 #-}
{-# RULES "dot_12_21" dot = dot_12_21 #-}
dot_12_21 :: V2 (S x Float) -> V2 (S x Float) -> S x Float
dot_12_21 a b = fun2 GlFloat "dot" (fromVec2 a) (fromVec2 b)

{-# INLINE dot_13_31 #-}
{-# RULES "dot_13_31" dot = dot_13_31 #-}
dot_13_31 :: V3 (S x Float) -> V3 (S x Float) -> S x Float
dot_13_31 a b = fun2 GlFloat "dot" (fromVec3 a) (fromVec3 b)

{-# INLINE dot_14_41 #-}
{-# RULES "dot_14_41" dot = dot_14_41 #-}
dot_14_41 :: V4 (S x Float) -> V4 (S x Float) -> S x Float
dot_14_41 a b = fun2 GlFloat "dot" (fromVec4 a) (fromVec4 b)

--
-- Matrix products
--
{-# INLINE mul_12_21vm #-}
{-# RULES "mul_12_21vm" (*!) = mul_12_21vm #-}
mul_12_21vm :: V2 (S x Float) -> V2 (V1 (S x Float)) -> V1 (S x Float)
mul_12_21vm a b = V1 $ fun2 GlFloat "dot" (fromVec2 a) (fromVec2 $ fmap unV1 b)

{-# INLINE mul_13_31vm #-}
{-# RULES "mul_13_31vm" (*!) = mul_13_31vm #-}
mul_13_31vm :: V3 (S x Float) -> V3 (V1 (S x Float)) -> V1 (S x Float)
mul_13_31vm a b = V1 $ fun2 GlFloat "dot" (fromVec3 a) (fromVec3 $ fmap unV1 b)

{-# INLINE mul_14_41vm #-}
{-# RULES "mul_14_41vm" (*!) = mul_14_41vm #-}
mul_14_41vm :: V4 (S x Float) -> V4 (V1 (S x Float)) -> V1 (S x Float)
mul_14_41vm a b = V1 $ fun2 GlFloat "dot" (fromVec4 a) (fromVec4 $ fmap unV1 b)

{-# INLINE mul_12_21mv #-}
{-# RULES "mul_12_21mv" (!*) = mul_12_21mv #-}
mul_12_21mv :: V1 (V2 (S x Float)) -> V2 (S x Float) -> V1 (S x Float)
mul_12_21mv a b = V1 $ fun2 GlFloat "dot" (fromVec2 $ unV1 a) (fromVec2 b)

{-# INLINE mul_13_31mv #-}
{-# RULES "mul_13_31mv" (!*) = mul_13_31mv #-}
mul_13_31mv :: V1 (V3 (S x Float)) -> V3 (S x Float) -> V1 (S x Float)
mul_13_31mv a b = V1 $ fun2 GlFloat "dot" (fromVec3 $ unV1 a) (fromVec3 b)

{-# INLINE mul_14_41mv #-}
{-# RULES "mul_14_41mv" (!*) = mul_14_41mv #-}
mul_14_41mv :: V1 (V4 (S x Float)) -> V4 (S x Float) -> V1 (S x Float)
mul_14_41mv a b = V1 $ fun2 GlFloat "dot" (fromVec4 $ unV1 a) (fromVec4 b)

{-# INLINE mul_12_21mm #-}
{-# RULES "mul_12_21mm" (!*!) = mul_12_21mm #-}
mul_12_21mm :: V1 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V1 (V1 (S x Float))
mul_12_21mm a b = V1 $ V1 $ fun2 GlFloat "dot" (fromVec2 $ unV1 a) (fromVec2 $ fmap unV1 b)

{-# INLINE mul_13_31mm #-}
{-# RULES "mul_13_31mm" (!*!) = mul_13_31mm #-}
mul_13_31mm :: V1 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V1 (V1 (S x Float))
mul_13_31mm a b = V1 $ V1 $ fun2 GlFloat "dot" (fromVec3 $ unV1 a) (fromVec3 $ fmap unV1 b)

{-# INLINE mul_14_41mm #-}
{-# RULES "mul_14_41mm" (!*!) = mul_14_41mm #-}
mul_14_41mm :: V1 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V1 (V1 (S x Float))
mul_14_41mm a b = V1 $ V1 $ fun2 GlFloat "dot" (fromVec4 $ unV1 a) (fromVec4 $ fmap unV1 b)

{-# INLINE mul_21_12 #-}
{-# RULES "mul_21_12" outer = mul_21_12 #-}
mul_21_12 :: V2 (S x Float) -> V2 (S x Float) -> V2 (V2 (S x Float))
mul_21_12 a b = fmap toV2S . toV2S
                  . fun2 GlMat2x2 "outerProduct" (fromVec2 b) . fromVec2 $ a

{-# INLINE mul_21_13 #-}
{-# RULES "mul_21_13" outer = mul_21_13 #-}
mul_21_13 :: V2 (S x Float) -> V3 (S x Float) -> V2 (V3 (S x Float))
mul_21_13 a b = fmap toV3S . toV2S
                  . fun2 GlMat2x3 "outerProduct" (fromVec3 b) . fromVec2 $ a

{-# INLINE mul_21_14 #-}
{-# RULES "mul_21_14" outer = mul_21_14 #-}
mul_21_14 :: V2 (S x Float) -> V4 (S x Float) -> V2 (V4 (S x Float))
mul_21_14 a b = fmap toV4S . toV2S
                  . fun2 GlMat2x4 "outerProduct" (fromVec4 b) . fromVec2 $ a

{-# INLINE mul_31_12 #-}
{-# RULES "mul_31_12" outer = mul_31_12 #-}
mul_31_12 :: V3 (S x Float) -> V2 (S x Float) -> V3 (V2 (S x Float))
mul_31_12 a b = fmap toV2S . toV3S
                  . fun2 GlMat3x2 "outerProduct" (fromVec2 b) . fromVec3 $ a

{-# INLINE mul_31_13 #-}
{-# RULES "mul_31_13" outer = mul_31_13 #-}
mul_31_13 :: V3 (S x Float) -> V3 (S x Float) -> V3 (V3 (S x Float))
mul_31_13 a b = fmap toV3S . toV3S
                  . fun2 GlMat3x3 "outerProduct" (fromVec3 b) . fromVec3 $ a

{-# INLINE mul_31_14 #-}
{-# RULES "mul_31_14" outer = mul_31_14 #-}
mul_31_14 :: V3 (S x Float) -> V4 (S x Float) -> V3 (V4 (S x Float))
mul_31_14 a b = fmap toV4S . toV3S
                  . fun2 GlMat3x4 "outerProduct" (fromVec4 b) . fromVec3 $ a

{-# INLINE mul_41_12 #-}
{-# RULES "mul_41_12" outer = mul_41_12 #-}
mul_41_12 :: V4 (S x Float) -> V2 (S x Float) -> V4 (V2 (S x Float))
mul_41_12 a b = fmap toV2S . toV4S
                  . fun2 GlMat4x2 "outerProduct" (fromVec2 b) . fromVec4 $ a

{-# INLINE mul_41_13 #-}
{-# RULES "mul_41_13" outer = mul_41_13 #-}
mul_41_13 :: V4 (S x Float) -> V3 (S x Float) -> V4 (V3 (S x Float))
mul_41_13 a b = fmap toV3S . toV4S
                  . fun2 GlMat4x3 "outerProduct" (fromVec3 b) . fromVec4 $ a

{-# INLINE mul_41_14 #-}
{-# RULES "mul_41_14" outer = mul_41_14 #-}
mul_41_14 :: V4 (S x Float) -> V4 (S x Float) -> V4 (V4 (S x Float))
mul_41_14 a b = fmap toV4S . toV4S
                  . fun2 GlMat4x4 "outerProduct" (fromVec4 b) . fromVec4 $ a

{-# INLINE mul_21_12m #-}
{-# RULES "mul_21_12m" (!*!) = mul_21_12m #-}
mul_21_12m :: V2 (V1 (S x Float)) -> V1 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_21_12m a = mul_21_12 (fmap unV1 a) . unV1

{-# INLINE mul_21_13m #-}
{-# RULES "mul_21_13m" (!*!) = mul_21_13m #-}
mul_21_13m :: V2 (V1 (S x Float)) -> V1 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_21_13m a = mul_21_13 (fmap unV1 a) . unV1

{-# INLINE mul_21_14m #-}
{-# RULES "mul_21_14m" (!*!) = mul_21_14m #-}
mul_21_14m :: V2 (V1 (S x Float)) -> V1 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_21_14m a = mul_21_14 (fmap unV1 a) . unV1

{-# INLINE mul_31_12m #-}
{-# RULES "mul_31_12m" (!*!) = mul_31_12m #-}
mul_31_12m :: V3 (V1 (S x Float)) -> V1 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_31_12m a = mul_31_12 (fmap unV1 a) . unV1

{-# INLINE mul_31_13m #-}
{-# RULES "mul_31_13m" (!*!) = mul_31_13m #-}
mul_31_13m :: V3 (V1 (S x Float)) -> V1 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_31_13m a = mul_31_13 (fmap unV1 a) . unV1

{-# INLINE mul_31_14m #-}
{-# RULES "mul_31_14m" (!*!) = mul_31_14m #-}
mul_31_14m :: V3 (V1 (S x Float)) -> V1 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_31_14m a = mul_31_14 (fmap unV1 a) . unV1

{-# INLINE mul_41_12m #-}
{-# RULES "mul_41_12m" (!*!) = mul_41_12m #-}
mul_41_12m :: V4 (V1 (S x Float)) -> V1 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_41_12m a = mul_41_12 (fmap unV1 a) . unV1

{-# INLINE mul_41_13m #-}
{-# RULES "mul_41_13m" (!*!) = mul_41_13m #-}
mul_41_13m :: V4 (V1 (S x Float)) -> V1 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_41_13m a = mul_41_13 (fmap unV1 a) . unV1

{-# INLINE mul_41_14m #-}
{-# RULES "mul_41_14m" (!*!) = mul_41_14m #-}
mul_41_14m :: V4 (V1 (S x Float)) -> V1 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_41_14m a = mul_41_14 (fmap unV1 a) . unV1

{-# INLINE mul_12_22 #-}
{-# RULES "mul_12_22" (*!) = mul_12_22 #-}
mul_12_22 :: V2 (S x Float) -> V2 (V2 (S x Float)) -> V2 (S x Float)
mul_12_22 v m = mulToV2 (fromMat22 m) . fromVec2 $ v

{-# INLINE mul_13_32 #-}
{-# RULES "mul_13_32" (*!) = mul_13_32 #-}
mul_13_32 :: V3 (S x Float) -> V3 (V2 (S x Float)) -> V2 (S x Float)
mul_13_32 v m = mulToV2 (fromMat32 m) . fromVec3 $ v

{-# INLINE mul_14_42 #-}
{-# RULES "mul_14_42" (*!) = mul_14_42 #-}
mul_14_42 :: V4 (S x Float) -> V4 (V2 (S x Float)) -> V2 (S x Float)
mul_14_42 v m = mulToV2 (fromMat42 m) . fromVec4 $ v

{-# INLINE mul_12_23 #-}
{-# RULES "mul_12_23" (*!) = mul_12_23 #-}
mul_12_23 :: V2 (S x Float) -> V2 (V3 (S x Float)) -> V3 (S x Float)
mul_12_23 v m = mulToV3 (fromMat23 m) . fromVec2 $ v

{-# INLINE mul_13_33 #-}
{-# RULES "mul_13_33" (*!) = mul_13_33 #-}
mul_13_33 :: V3 (S x Float) -> V3 (V3 (S x Float)) -> V3 (S x Float)
mul_13_33 v m = mulToV3 (fromMat33 m) . fromVec3 $ v

{-# INLINE mul_14_43 #-}
{-# RULES "mul_14_43" (*!) = mul_14_43 #-}
mul_14_43 :: V4 (S x Float) -> V4 (V3 (S x Float)) -> V3 (S x Float)
mul_14_43 v m = mulToV3 (fromMat43 m) . fromVec4 $ v

{-# INLINE mul_12_24 #-}
{-# RULES "mul_12_24" (*!) = mul_12_24 #-}
mul_12_24 :: V2 (S x Float) -> V2 (V4 (S x Float)) -> V4 (S x Float)
mul_12_24 v m = mulToV4 (fromMat24 m) . fromVec2 $ v

{-# INLINE mul_13_34 #-}
{-# RULES "mul_13_34" (*!) = mul_13_34 #-}
mul_13_34 :: V3 (S x Float) -> V3 (V4 (S x Float)) -> V4 (S x Float)
mul_13_34 v m = mulToV4 (fromMat34 m) . fromVec3 $ v

{-# INLINE mul_14_44 #-}
{-# RULES "mul_14_44" (*!) = mul_14_44 #-}
mul_14_44 :: V4 (S x Float) -> V4 (V4 (S x Float)) -> V4 (S x Float)
mul_14_44 v m = mulToV4 (fromMat44 m) . fromVec4 $ v

{-# RULES "mul_12_22m" (!*!) = mul_12_22m #-}
{-# RULES "mul_13_32m" (!*!) = mul_13_32m #-}
{-# RULES "mul_14_42m" (!*!) = mul_14_42m #-}
{-# RULES "mul_12_23m" (!*!) = mul_12_23m #-}
{-# RULES "mul_13_33m" (!*!) = mul_13_33m #-}
{-# RULES "mul_14_43m" (!*!) = mul_14_43m #-}
{-# RULES "mul_12_24m" (!*!) = mul_12_24m #-}
{-# RULES "mul_13_34m" (!*!) = mul_13_34m #-}
{-# RULES "mul_14_44m" (!*!) = mul_14_44m #-}
mul_12_22m :: V1 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V1 (V2 (S x Float))
mul_12_22m v m = V1 $ mulToV2 (fromMat22 m) (fromVec2 $ unV1 v)
mul_13_32m :: V1 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V1 (V2 (S x Float))
mul_13_32m v m = V1 $ mulToV2 (fromMat32 m) (fromVec3 $ unV1 v)
mul_14_42m :: V1 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V1 (V2 (S x Float))
mul_14_42m v m = V1 $ mulToV2 (fromMat42 m) (fromVec4 $ unV1 v)
mul_12_23m :: V1 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V1 (V3 (S x Float))
mul_12_23m v m = V1 $ mulToV3 (fromMat23 m) (fromVec2 $ unV1 v)
mul_13_33m :: V1 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V1 (V3 (S x Float))
mul_13_33m v m = V1 $ mulToV3 (fromMat33 m) (fromVec3 $ unV1 v)
mul_14_43m :: V1 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V1 (V3 (S x Float))
mul_14_43m v m = V1 $ mulToV3 (fromMat43 m) (fromVec4 $ unV1 v)
mul_12_24m :: V1 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V1 (V4 (S x Float))
mul_12_24m v m = V1 $ mulToV4 (fromMat24 m) (fromVec2 $ unV1 v)
mul_13_34m :: V1 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V1 (V4 (S x Float))
mul_13_34m v m = V1 $ mulToV4 (fromMat34 m) (fromVec3 $ unV1 v)
mul_14_44m :: V1 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V1 (V4 (S x Float))
mul_14_44m v m = V1 $ mulToV4 (fromMat44 m) (fromVec4 $ unV1 v)

{-# RULES "mul_22_21" (!*) = mul_22_21 #-}
{-# RULES "mul_23_31" (!*) = mul_23_31 #-}
{-# RULES "mul_24_41" (!*) = mul_24_41 #-}
{-# RULES "mul_32_21" (!*) = mul_32_21 #-}
{-# RULES "mul_33_31" (!*) = mul_33_31 #-}
{-# RULES "mul_34_41" (!*) = mul_34_41 #-}
{-# RULES "mul_42_21" (!*) = mul_42_21 #-}
{-# RULES "mul_43_31" (!*) = mul_43_31 #-}
{-# RULES "mul_44_41" (!*) = mul_44_41 #-}
mul_22_21 :: V2 (V2 (S x Float)) -> V2 (S x Float) -> V2 (S x Float)
mul_22_21 m v = mulToV2 (fromVec2 v) (fromMat22 m)
mul_23_31 :: V2 (V3 (S x Float)) -> V3 (S x Float) -> V2 (S x Float)
mul_23_31 m v = mulToV2 (fromVec3 v) (fromMat23 m)
mul_24_41 :: V2 (V4 (S x Float)) -> V4 (S x Float) -> V2 (S x Float)
mul_24_41 m v = mulToV2 (fromVec4 v) (fromMat24 m)
mul_32_21 :: V3 (V2 (S x Float)) -> V2 (S x Float) -> V3 (S x Float)
mul_32_21 m v = mulToV3 (fromVec2 v) (fromMat32 m)
mul_33_31 :: V3 (V3 (S x Float)) -> V3 (S x Float) -> V3 (S x Float)
mul_33_31 m v = mulToV3 (fromVec3 v) (fromMat33 m)
mul_34_41 :: V3 (V4 (S x Float)) -> V4 (S x Float) -> V3 (S x Float)
mul_34_41 m v = mulToV3 (fromVec4 v) (fromMat34 m)
mul_42_21 :: V4 (V2 (S x Float)) -> V2 (S x Float) -> V4 (S x Float)
mul_42_21 m v = mulToV4 (fromVec2 v) (fromMat42 m)
mul_43_31 :: V4 (V3 (S x Float)) -> V3 (S x Float) -> V4 (S x Float)
mul_43_31 m v = mulToV4 (fromVec3 v) (fromMat43 m)
mul_44_41 :: V4 (V4 (S x Float)) -> V4 (S x Float) -> V4 (S x Float)
mul_44_41 m v = mulToV4 (fromVec4 v) (fromMat44 m)

{-# RULES "mul_22_21m" (!*!) = mul_22_21m #-}
{-# RULES "mul_23_31m" (!*!) = mul_23_31m #-}
{-# RULES "mul_24_41m" (!*!) = mul_24_41m #-}
{-# RULES "mul_32_21m" (!*!) = mul_32_21m #-}
{-# RULES "mul_33_31m" (!*!) = mul_33_31m #-}
{-# RULES "mul_34_41m" (!*!) = mul_34_41m #-}
{-# RULES "mul_42_21m" (!*!) = mul_42_21m #-}
{-# RULES "mul_43_31m" (!*!) = mul_43_31m #-}
{-# RULES "mul_44_41m" (!*!) = mul_44_41m #-}
mul_22_21m :: V2 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V2 (V1 (S x Float))
mul_22_21m m v = V1 <$> mulToV2 (fromVec2 $ fmap unV1 v) (fromMat22 m)
mul_23_31m :: V2 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V2 (V1 (S x Float))
mul_23_31m m v = V1 <$> mulToV2 (fromVec3 $ fmap unV1 v) (fromMat23 m)
mul_24_41m :: V2 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V2 (V1 (S x Float))
mul_24_41m m v = V1 <$> mulToV2 (fromVec4 $ fmap unV1 v) (fromMat24 m)
mul_32_21m :: V3 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V3 (V1 (S x Float))
mul_32_21m m v = V1 <$> mulToV3 (fromVec2 $ fmap unV1 v) (fromMat32 m)
mul_33_31m :: V3 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V3 (V1 (S x Float))
mul_33_31m m v = V1 <$> mulToV3 (fromVec3 $ fmap unV1 v) (fromMat33 m)
mul_34_41m :: V3 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V3 (V1 (S x Float))
mul_34_41m m v = V1 <$> mulToV3 (fromVec4 $ fmap unV1 v) (fromMat34 m)
mul_42_21m :: V4 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V4 (V1 (S x Float))
mul_42_21m m v = V1 <$> mulToV4 (fromVec2 $ fmap unV1 v) (fromMat42 m)
mul_43_31m :: V4 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V4 (V1 (S x Float))
mul_43_31m m v = V1 <$> mulToV4 (fromVec3 $ fmap unV1 v) (fromMat43 m)
mul_44_41m :: V4 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V4 (V1 (S x Float))
mul_44_41m m v = V1 <$> mulToV4 (fromVec4 $ fmap unV1 v) (fromMat44 m)

{-# INLINE mul_22_22 #-}
{-# RULES "mul_22_22" (!*!) = mul_22_22 #-}
mul_22_22 :: V2 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_22_22 a b = fmap toV2S . toV2S . bin GlMat2x2 "*" (fromMat22 b) . fromMat22 $ a

{-# INLINE mul_23_32 #-}
{-# RULES "mul_23_32" (!*!) = mul_23_32 #-}
mul_23_32 :: V2 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_23_32 a b = fmap toV2S . toV2S . bin GlMat2x2 "*" (fromMat32 b) . fromMat23 $ a

{-# INLINE mul_24_42 #-}
{-# RULES "mul_24_42" (!*!) = mul_24_42 #-}
mul_24_42 :: V2 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_24_42 a b = fmap toV2S . toV2S . bin GlMat2x2 "*" (fromMat42 b) . fromMat24 $ a

{-# INLINE mul_22_23 #-}
{-# RULES "mul_22_23" (!*!) = mul_22_23 #-}
mul_22_23 :: V2 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_22_23 a b = fmap toV3S . toV2S . bin GlMat2x3 "*" (fromMat23 b) . fromMat22 $ a

{-# INLINE mul_23_33 #-}
{-# RULES "mul_23_33" (!*!) = mul_23_33 #-}
mul_23_33 :: V2 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_23_33 a b = fmap toV3S . toV2S . bin GlMat2x3 "*" (fromMat33 b) . fromMat23 $ a

{-# INLINE mul_24_43 #-}
{-# RULES "mul_24_43" (!*!) = mul_24_43 #-}
mul_24_43 :: V2 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_24_43 a b = fmap toV3S . toV2S . bin GlMat2x3 "*" (fromMat43 b) . fromMat24 $ a

{-# INLINE mul_22_24 #-}
{-# RULES "mul_22_24" (!*!) = mul_22_24 #-}
mul_22_24 :: V2 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_22_24 a b = fmap toV4S . toV2S . bin GlMat2x4 "*" (fromMat24 b) . fromMat22 $ a

{-# INLINE mul_23_34 #-}
{-# RULES "mul_23_34" (!*!) = mul_23_34 #-}
mul_23_34 :: V2 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_23_34 a b = fmap toV4S . toV2S . bin GlMat2x4 "*" (fromMat34 b) . fromMat23 $ a

{-# INLINE mul_24_44 #-}
{-# RULES "mul_24_44" (!*!) = mul_24_44 #-}
mul_24_44 :: V2 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_24_44 a b = fmap toV4S . toV2S . bin GlMat2x4 "*" (fromMat44 b) . fromMat24 $ a

{-# INLINE mul_32_22 #-}
{-# RULES "mul_32_22" (!*!) = mul_32_22 #-}
mul_32_22 :: V3 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_32_22 a b = fmap toV2S . toV3S . bin GlMat3x2 "*" (fromMat22 b) . fromMat32 $ a

{-# INLINE mul_33_32 #-}
{-# RULES "mul_33_32" (!*!) = mul_33_32 #-}
mul_33_32 :: V3 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_33_32 a b = fmap toV2S . toV3S . bin GlMat3x2 "*" (fromMat32 b) . fromMat33 $ a

{-# INLINE mul_34_42 #-}
{-# RULES "mul_34_42" (!*!) = mul_34_42 #-}
mul_34_42 :: V3 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_34_42 a b = fmap toV2S . toV3S . bin GlMat3x2 "*" (fromMat42 b) . fromMat34 $ a

{-# INLINE mul_32_23 #-}
{-# RULES "mul_32_23" (!*!) = mul_32_23 #-}
mul_32_23 :: V3 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_32_23 a b = fmap toV3S . toV3S . bin GlMat3x3 "*" (fromMat23 b) . fromMat32 $ a

{-# INLINE mul_33_33 #-}
{-# RULES "mul_33_33" (!*!) = mul_33_33 #-}
mul_33_33 :: V3 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_33_33 a b = fmap toV3S . toV3S . bin GlMat3x3 "*" (fromMat33 b) . fromMat33 $ a

{-# INLINE mul_34_43 #-}
{-# RULES "mul_34_43" (!*!) = mul_34_43 #-}
mul_34_43 :: V3 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_34_43 a b = fmap toV3S . toV3S . bin GlMat3x3 "*" (fromMat43 b) . fromMat34 $ a

{-# INLINE mul_32_24 #-}
{-# RULES "mul_32_24" (!*!) = mul_32_24 #-}
mul_32_24 :: V3 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_32_24 a b = fmap toV4S . toV3S . bin GlMat3x4 "*" (fromMat24 b) . fromMat32 $ a

{-# INLINE mul_33_34 #-}
{-# RULES "mul_33_34" (!*!) = mul_33_34 #-}
mul_33_34 :: V3 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_33_34 a b = fmap toV4S . toV3S . bin GlMat3x4 "*" (fromMat34 b) . fromMat33 $ a

{-# INLINE mul_34_44 #-}
{-# RULES "mul_34_44" (!*!) = mul_34_44 #-}
mul_34_44 :: V3 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_34_44 a b = fmap toV4S . toV3S . bin GlMat3x4 "*" (fromMat44 b) . fromMat34 $ a

{-# INLINE mul_42_22 #-}
{-# RULES "mul_42_22" (!*!) = mul_42_22 #-}
mul_42_22 :: V4 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_42_22 a b = fmap toV2S . toV4S . bin GlMat4x2 "*" (fromMat22 b) . fromMat42 $ a

{-# INLINE mul_43_32 #-}
{-# RULES "mul_43_32" (!*!) = mul_43_32 #-}
mul_43_32 :: V4 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_43_32 a b = fmap toV2S . toV4S . bin GlMat4x2 "*" (fromMat32 b) . fromMat43 $ a

{-# INLINE mul_44_42 #-}
{-# RULES "mul_44_42" (!*!) = mul_44_42 #-}
mul_44_42 :: V4 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_44_42 a b = fmap toV2S . toV4S . bin GlMat4x2 "*" (fromMat42 b) . fromMat44 $ a

{-# INLINE mul_42_23 #-}
{-# RULES "mul_42_23" (!*!) = mul_42_23 #-}
mul_42_23 :: V4 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_42_23 a b = fmap toV3S . toV4S . bin GlMat4x3 "*" (fromMat23 b) . fromMat42 $ a

{-# INLINE mul_43_33 #-}
{-# RULES "mul_43_33" (!*!) = mul_43_33 #-}
mul_43_33 :: V4 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_43_33 a b = fmap toV3S . toV4S . bin GlMat4x3 "*" (fromMat33 b) . fromMat43 $ a

{-# INLINE mul_44_43 #-}
{-# RULES "mul_44_43" (!*!) = mul_44_43 #-}
mul_44_43 :: V4 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_44_43 a b = fmap toV3S . toV4S . bin GlMat4x3 "*" (fromMat43 b) . fromMat44 $ a

{-# INLINE mul_42_24 #-}
{-# RULES "mul_42_24" (!*!) = mul_42_24 #-}
mul_42_24 :: V4 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_42_24 a b = fmap toV4S . toV4S . bin GlMat4x4 "*" (fromMat24 b) . fromMat42 $ a

{-# INLINE mul_43_34 #-}
{-# RULES "mul_43_34" (!*!) = mul_43_34 #-}
mul_43_34 :: V4 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_43_34 a b = fmap toV4S . toV4S . bin GlMat4x4 "*" (fromMat34 b) . fromMat43 $ a

{-# INLINE mul_44_44 #-}
{-# RULES "mul_44_44" (!*!) = mul_44_44 #-}
mul_44_44 :: V4 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_44_44 a b = fmap toV4S . toV4S . bin GlMat4x4 "*" (fromMat44 b) . fromMat44 $ a
