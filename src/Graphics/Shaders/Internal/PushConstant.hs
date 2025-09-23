module Graphics.Shaders.Internal.PushConstant (
  getPushConstant
) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString.Char8 as BS
import Data.Maybe
import Foreign hiding (void)
import Linear

import Control.Monad.State.Extra
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.Pipeline

class PushConstant a where
  type PushFormat a x
  toPushConstant :: ToPush x a (PushFormat a x)

data ToPush x a b = ToPush
  -- GLSL declaration builder.
  (Kleisli DeclM a b)
  -- Calculate size of and offsets
  (Kleisli (State (Int, [Int])) a b)
  -- Value writer
  (Kleisli (StateT (Ptr (), [Int]) IO) a b)

instance Category (ToPush x) where
  id = ToPush id id id
  (ToPush a b c) . (ToPush a' b' c') = ToPush (a . a') (b . b') (c . c')

instance Arrow (ToPush x) where
  arr f = ToPush (arr f) (arr f) (arr f)
  first (ToPush a b c) = ToPush (first a) (first b) (first c)


newtype DeclM a = DeclM { unDeclM :: StateT Int (Writer [ByteString]) a }
 deriving (Functor, Applicative, Monad, MonadState Int,
   MonadWriter [ByteString])

runDeclM :: DeclM a -> (a, [ByteString])
runDeclM = runWriter . flip evalStateT 0 . unDeclM


getPushConstant
  :: forall t e c x. (PushConstant c)
  => PipelineBuilder t c e (PushFormat c x)
getPushConstant = do
  mBinding <- PipelineBuilder . gets $ \(_, _, _, _, _, pc) -> pc

  let ToPush (Kleisli buildDecl) (Kleisli calcSize) (Kleisli valWriter')
        = toPushConstant :: ToPush x c (PushFormat c x)
      (a, decls) = runDeclM . buildDecl $ (undefined :: c)

      decl = "layout(push_constant) uniform constants"
        <> " {\n"
        <> "  " <> intercalate "\n  " decls <> "\n"
        <> "} pc;"

      (sz, pads) = flip execState (0, []) . calcSize $ undefined

      valWriter ptr = void . flip runStateT (ptr, pads) . valWriter'

  when (isNothing mBinding) $ do
    let binding = PushConstantBinding decl sz valWriter
    PipelineBuilder . modify $ \(n, un, ins, ubs, sbs, _) ->
      (n, un, ins, ubs, sbs, Just binding)

  return a

instance PushConstant () where
  type PushFormat () x = ()
  toPushConstant = arr $ const ()

toPushUnaligned
  :: forall x a. (Storable a)
  => ByteString -> ToPush x a (ExprM ByteString)
toPushUnaligned typ = ToPush declare calcSize pokeVal
 where
  declare = Kleisli $ \_ -> do
    n <- BS.pack . show <$> getNext
    tell [ typ <> " _" <> n <> ";" ]
    return . return $ "pc._" <> n

  calcSize = Kleisli $ \_ -> do
    modify $ \(sz, pads) ->
      let sz' = sizeOf (undefined :: a)
      in (sz + sz', pads)
    return undefined

  pokeVal = Kleisli $ \a -> do
    (ptr, pads) <- get
    let sz = sizeOf a
    put (ptr `plusPtr` sz, pads)
    liftIO $ poke (castPtr ptr) a
    return undefined

instance PushConstant Float where
  type PushFormat Float x = S x Float
  toPushConstant = toPushUnaligned "float" >>> arr S

instance PushConstant a => PushConstant (V4 a) where
  type PushFormat (V4 a) x = V4 (PushFormat a x)
  toPushConstant = proc ~(V4 a b c d) -> do
    (a', b', c', d') <- toPushConstant -< (a, b, c, d)
    returnA -< V4 a' b' c' d'

instance (PushConstant a, PushConstant b, PushConstant c, PushConstant d)
    => PushConstant (a, b, c, d) where
  type PushFormat (a, b, c, d) x =
    (PushFormat a x, PushFormat b x, PushFormat c x, PushFormat d x)
  toPushConstant = proc ~(a, b, c, d) -> do
    (a', b', c') <- toPushConstant -< (a, b, c)
    d' <- toPushConstant -< d
    returnA -< (a', b', c', d')

instance (PushConstant a, PushConstant b, PushConstant c)
    => PushConstant (a, b, c) where
  type PushFormat (a, b, c) x =
    (PushFormat a x, PushFormat b x, PushFormat c x)
  toPushConstant = proc ~(a, b, c) -> do
    (a', b') <- toPushConstant -< (a, b)
    c' <- toPushConstant -< c
    returnA -< (a', b', c')

instance (PushConstant a, PushConstant b)
    => PushConstant (a, b) where
  type PushFormat (a, b) x = (PushFormat a x, PushFormat b x)
  toPushConstant = proc ~(a, b) -> do
    a' <- toPushConstant -< a
    b' <- toPushConstant -< b
    returnA -< (a', b')
