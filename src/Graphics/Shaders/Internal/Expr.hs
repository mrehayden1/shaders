module Graphics.Shaders.Internal.Expr (
  S(..),
  V,
  F,

  ExprM(..),

  execExprM,
  tellStatement,

  vec2,
  vec3,
  vec4,

  mat3,
  mat4
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Monad.State.Extra
import Data.Linear

-- Shader expression of type `a` usable in shaders of type `x`.
newtype S x a = S { unS :: ExprM ByteString }

-- Phantom type representing vertex shaders
data V = V

-- Phantom type representing fragment shaders
data F = F

newtype ExprM a = ExprM {
  unExprM :: ReaderT Indent (StateT TempName (Writer ByteString)) a
} deriving (Functor, Applicative, Monad, MonadReader Indent)

type Indent = Int
type TempName = Int

execExprM :: Int -> ExprM a -> ByteString
execExprM n = execWriter . flip evalStateT 0 . flip runReaderT n . unExprM

tellStatement :: ByteString -> ExprM ()
tellStatement s = do
  pad <- asks $ BS.pack . flip replicate ' '
  ExprM . lift . lift . tell $ pad <> s <> ";\n"

tellAssignment :: ByteString -> ByteString -> ExprM ByteString
tellAssignment t s = do
  n <- ("t" <>) . BS.pack . show <$> ExprM getNext
  tellStatement $ t <> " " <> n <> " = " <> s
  return n

bin :: ByteString -> ByteString -> S x a -> S x a -> S x a
bin typ op a b = S $ do
  a' <- unS a
  b' <- unS b
  tellAssignment typ $ "(" <> a' <> " " <> op <> " " <> b' <> ")"

fun1 :: ByteString -> ByteString -> S x a -> S x a
fun1 typ fn a = S $ do
  a' <- unS a
  tellAssignment typ $ fn <> "(" <> a' <> ")"

fun2 :: ByteString -> ByteString -> S x a -> S x a -> S x a
fun2 typ fn a b = S $ do
  a' <- unS a
  b' <- unS b
  tellAssignment typ $ fn <> "(" <> a' <> ", " <> b' <> ")"

preop :: ByteString -> ByteString -> S x a -> S x a
preop typ op a = S $ do
  a' <- unS a
  tellAssignment typ $ "(" <> op <> a' <> ")"

postop :: ByteString -> ByteString -> S x a -> S x a
postop typ op a = S $ do
  a' <- unS a
  tellAssignment typ $ "(" <> a' <> op <> ")"

vec2 :: S x a -> S x a -> S x (V4 a)
vec2 a b = S $ do
  a' <- unS a
  b' <- unS b
  tellAssignment "vec2" $
    "vec2(" <> a' <> ", " <> b' <> ")"

vec3 :: S x a -> S x a -> S x a -> S x (V4 a)
vec3 a b c = S $ do
  a' <- unS a
  b' <- unS b
  c' <- unS c
  tellAssignment "vec3" $
    "vec3(" <> a' <> ", " <> b' <> ", " <> c' <> ")"

vec4 :: S x a -> S x a -> S x a -> S x a -> S x (V4 a)
vec4 a b c d = S $ do
  a' <- unS a
  b' <- unS b
  c' <- unS c
  d' <- unS d
  tellAssignment "vec4" $
    "vec4(" <> a' <> ", " <> b' <> ", " <> c' <> ", " <> d' <> ")"

mat3 :: S x (V3 a) -> S x (V3 a) -> S x (V3 a) -> S x (M33 a)
mat3 a b c = S $ do
  a' <- unS a
  b' <- unS b
  c' <- unS c
  tellAssignment "mat3" $
    "mat3(" <> a' <> ", " <> b' <> ", " <> c' <> ")"

mat4 :: S x (V4 a) -> S x (V4 a) -> S x (V4 a) -> S x (V4 a) -> S x (M44 a)
mat4 a b c d = S $ do
  a' <- unS a
  b' <- unS b
  c' <- unS c
  d' <- unS d
  tellAssignment "mat4" $
    "mat4(" <> a' <> ", " <> b' <> ", " <> c' <> ", " <> d' <> ")"

instance Num (S x Float) where
  (+) = bin "float" "+"
  (-) = bin "float" "-"
  (*) = bin "float" "*"
  abs = fun1 "float" "abs"
  signum = fun1 "float" "sign"
  negate = preop "float" "-"

  fromInteger n = S $ do
    return . BS.pack . show $ n

instance Fractional (S x Float) where
  (/) = bin "float" "/"
  fromRational = S . return . ("float(" <>) . (<> ")") . BS.pack . show
    . (`asTypeOf` (undefined :: Float)) . fromRational

instance Floating (S x Float) where
  pi    = S $ return . BS.pack . show $ (pi :: Float)
  sqrt  = fun1 "float" "sqrt"
  exp   = fun1 "float" "exp"
  log   = fun1 "float" "log"
  (**)  = fun2 "float" "pow"
  sin   = fun1 "float" "sin"
  cos   = fun1 "float" "cos"
  tan   = fun1 "float" "tan"
  asin  = fun1 "float" "asin"
  acos  = fun1 "float" "acos"
  atan  = fun1 "float" "atan"
  sinh  = fun1 "float" "sinh"
  cosh  = fun1 "float" "cosh"
  asinh = fun1 "float" "asinh"
  atanh = fun1 "float" "atanh"
  acosh = fun1 "float" "acosh"


-- Basis vector accesors

instance R1 (S x (V2 a)) (S x a) where
  _x (S a) = S $ do
    n <- a
    return $ n <> ".x"

instance R1 (S x (V3 a)) (S x a) where
  _x (S a) = S $ do
    n <- a
    return $ n <> ".x"

instance R1 (S x (V4 a)) (S x a) where
  _x (S a) = S $ do
    n <- a
    return $ n <> ".x"

instance R1 (S x (M33 a)) (S x (V3 a)) where
  _0 (S a) = S $ do
    n <- a
    return $ n <> "[0]"

instance R1 (S x (M44 a)) (S x (V4 a)) where
  _0 (S a) = S $ do
    n <- a
    return $ n <> "[0]"


instance R2 (S x (V2 a)) (S x a) where
  _y (S a) = S $ do
    n <- a
    return $ n <> ".y"

instance R2 (S x (V3 a)) (S x a) where
  _y (S a) = S $ do
    n <- a
    return $ n <> ".y"

instance R2 (S x (V4 a)) (S x a) where
  _y (S a) = S $ do
    n <- a
    return $ n <> ".y"

instance R2 (S x (M33 a)) (S x (V3 a)) where
  _1 (S a) = S $ do
    n <- a
    return $ n <> "[1]"

instance R2 (S x (M44 a)) (S x (V4 a)) where
  _1 (S a) = S $ do
    n <- a
    return $ n <> "[1]"


instance R3 (S x (V3 a)) (S x a) where
  _z (S a) = S $ do
    n <- a
    return $ n <> ".z"

instance R3 (S x (V4 a)) (S x a) where
  _z (S a) = S $ do
    n <- a
    return $ n <> ".z"

instance R3 (S x (M33 a)) (S x (V3 a)) where
  _2 (S a) = S $ do
    n <- a
    return $ n <> "[2]"

instance R3 (S x (M44 a)) (S x (V4 a)) where
  _2 (S a) = S $ do
    n <- a
    return $ n <> "[2]"


instance R4 (S x (V4 a)) (S x a) where
  _w (S a) = S $ do
    n <- a
    return $ n <> ".w"

instance R4 (S x (M44 a)) (S x (V4 a)) where
  _3 (S a) = S $ do
    n <- a
    return $ n <> "[3]"
