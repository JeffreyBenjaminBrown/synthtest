{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, BangPatterns,
  FlexibleContexts, FunctionalDependencies, ScopedTypeVariables,
  TypeFamilies #-}

module Lib
  ( sineWave
  , runSignal
  , Interpreter(..)
  , AccSample(..)
  , Signal
  , funToTable
  , Clock(..)
  , AudRate
  , Table(..)
  , Backend(..)
  ) where
import Control.Arrow 
import Control.Arrow.Operations
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, natVal, Nat)
import Control.Category
import Utils
import Debug.Trace (trace)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.LLVM.Native as AL
import qualified Data.Array.Accelerate.Array.Sugar as AS hiding (Elt)


data AudRate

newtype SF a b = SF
  { runSF :: (a -> (b, SF a b))
  }

newtype ArrowP a p b c = ArrowP
  { strip :: a b c
  }

instance Category a => Category (ArrowP a p) where
  id = ArrowP Control.Category.id
  ArrowP g . ArrowP f = ArrowP (g Control.Category.. f)

instance Arrow a => Arrow (ArrowP a p) where
  arr f = ArrowP (arr f)
  first (ArrowP f) = ArrowP (first f)

instance ArrowLoop a => ArrowLoop (ArrowP a p) where
  loop (ArrowP f) = ArrowP (loop f)
   
instance ArrowCircuit a => ArrowCircuit (ArrowP a p) where
  delay i = ArrowP (delay i)

instance ArrowChoice a => ArrowChoice (ArrowP a p) where
  left (ArrowP f) = ArrowP (left f)
  ArrowP f ||| ArrowP g = ArrowP (f ||| g)

instance Category SF where
  id = SF h
    where
      h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in f' `seq` g' `seq` (z, SF (h f' g'))

instance Arrow SF where
  arr f = g
    where
      g = SF (\x -> (f x, g))
  first f = SF (g f)
    where
      g f (x, z) = f' `seq` ((y, z), SF (g f'))
        where
          (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x)
        in ((y, z), SF (h f' g'))

instance ArrowLoop SF where
  loop sf = SF (g sf)
    where
      g f x = f' `seq` (y, SF (g f'))
        where
          ((y, z), f') = runSF f (x, z)

instance ArrowChoice SF where
  left sf = SF (g sf)
    where
      g f x =
        case x of
          Left a ->
            let (y, f') = runSF f a
            in f' `seq` (Left y, SF (g f'))
          Right b -> (Right b, SF (g f))

instance ArrowCircuit SF where
  delay i = SF (f i)
    where
      f i x = (i, SF (f x))

type Signal clk a b = ArrowP SF clk a b
type SigFun clk a b = ArrowP SF clk a b

-- Arbitrary number of channels (say, 5.1) can be supported by just adding more
-- instances of the AudioSample type class.

class AudioSample a where
  zero :: a
  mix :: a -> a -> a
  collapse :: a -> [Double]
  numChans :: a -> Int
      -- allows us to reify the number of channels from the type.
      
type SEvent a = Maybe a

class Clock p a where
  rate :: p -> A.Exp a -- sampling rate
    
data Table a =
  Table !(A.Acc (A.Scalar Int)) -- size
        !(A.Acc (A.Array A.DIM1 a)) -- table implementation
        !(A.Acc (A.Scalar Bool)) -- Whether the table is normalized

instance (A.Elt a) => Show (Table a) where
  show (Table sz a n) = "Table with " ++ show sz ++ " entries: " ++ show a

funToTable ::
     (A.Num a, A.Ord a, A.Fractional a, A.FromIntegral Int a)
  => (A.Exp a -> A.Exp a)
  -> A.Acc (A.Scalar Bool)
  -> A.Acc (A.Scalar Int)
  -> Table a
funToTable f normalize size =
  let xs =
        A.enumFromStepN
          ((A.index1 (A.the size)))
          0
          (A.recip $ A.fromIntegral (A.the size)) -- :: A.Acc (A.Array A.DIM1 a)
      ys = A.map f xs -- :: A.Acc (A.Array A.DIM1 a)
      zs = A.acond (A.the normalize) (A.map (A./ (maxabs ys)) ys) ys
      maxabs =
        A.the Prelude.. A.maximum Prelude.. A.map A.abs -- :: A.Acc (A.Array A.DIM1 a) -> A.Exp a
  in Table size zs normalize

data Backend
  = LLVM
  | CUDA

data AccSample (i :: Backend) (l :: Nat) a =
  AccSample (A.Acc (A.Array A.DIM1 a)) 
getLength ::
     forall i l a. (KnownNat l)
  => (AccSample i l a)
  -> Int
getLength _ = fromIntegral $ natVal (Proxy :: Proxy l) :: Int

fillLength ::
     (A.Shape (A.Z A.:. l), A.Elt e)
  => l
  -> A.Exp e
  -> A.Acc (A.Array (A.Z A.:. l) e)
fillLength l v =
    A.fill (A.constant (A.Z A.:. l)) v

type Numeric a
   = ( A.Elt a
     , A.RealFrac a
     , Fractional (A.Exp a)
     , Num a
     , Floating (A.Exp a)
     , A.FromIntegral Int a)

instance forall i l a. (KnownNat l, Numeric a) =>
         AudioSample (AccSample i l a) where
  zero =
    let s = fromIntegral $ natVal (Proxy :: Proxy l) :: Int
        z = fillLength s 0.0
    in AccSample z
  mix (AccSample a) (AccSample b) = (AccSample $ A.zipWith (A.+) a b)
  collapse (AccSample a) = [2.0] -- float2Double $ A.toList . A.run $ a
  numChans _ = 1

instance forall i l a. (KnownNat l, A.Elt a, Fractional (A.Exp a)) =>
         AudioSample (AccSample i l a, AccSample i l a) where
  zero =
    let s = (fromIntegral $ natVal (Proxy :: Proxy l)) :: Int
        z = A.fill (A.constant (A.Z A.:. s)) 0.0
    in (AccSample z, AccSample z)
  mix (AccSample a, AccSample b) (AccSample c, AccSample d) =
    (AccSample $ A.zipWith (A.+) a c, AccSample $ A.zipWith (A.+) b d)
  collapse (AccSample a, AccSample b) = [2.0]
  numChans _ = 2

-- run a signal with an accelerate interpreter
runSignal ::
     Interpreter i
  => Signal c b (AccSample i l a)
  -> b
  -> A.Acc (A.Array A.DIM1 a)
runSignal s i =
    let (AccSample r, SF _) = runSF (strip s) i
    in r

-- Tell accelerate to run a signal that is parameterized by A.Acc b and which outputs an AccSample i l a to a storable vector.
class Interpreter i where
  runToStorable ::
       (A.Arrays b, Num a, A.Elt a)
    => Signal c (A.Acc b) (AccSample i l a)
    -> b
    -> AIO.Vectors (AS.EltRepr a)
  evalToStorable ::
       (Num a, A.Elt a)
    => Signal c () (AccSample i l a)
    -> AIO.Vectors (AS.EltRepr a)

instance Interpreter CUDA where
  runToStorable sig n = AIO.toVectors $ AI.run1 (runSignal sig) n
  evalToStorable sig = AIO.toVectors $ AI.run (runSignal sig ()) 

instance Interpreter LLVM where
  runToStorable sig n = AIO.toVectors $ AL.run1 (runSignal sig) n
  evalToStorable sig = AIO.toVectors $ AL.run (runSignal sig ())


wrap
    :: (A.Ord n, Num (A.Exp n))
    =>A.Exp  n -> A.Exp n ->A.Exp  n
wrap val bound =
    A.ifThenElse (val A.> bound)
        (wrap val (val - bound))
        val
clip
    :: Ord (A.Exp n)
    => A.Exp  n ->A.Exp  n ->A.Exp  n ->A.Exp  n
clip val lower upper
  | val <= lower = lower
  | val >= upper = upper
  | otherwise = val

pow
    :: Floating (A.Exp a)
    => A.Exp a -> A.Exp a -> A.Exp a
pow a b = A.exp (A.log a A.* b)

--Reads a window of size modu starting at pos from the table.  wraps the window as needed
-- change this to take start and end phase
readFromTable
    :: forall i l a.
       (KnownNat l, Numeric a)
    => Table a -> A.Exp a -> (AccSample i l a)
readFromTable (Table sz array _) pos =
    let modu =
            A.the $ A.unit (fromIntegral $ natVal (Proxy :: Proxy l)) :: A.Exp Int
        idx = A.truncate (A.fromIntegral (A.the sz) A.* pos)
        idx2 = (idx + modu)
        wrp =
            A.the $
            A.zipWith (A.-) sz (A.zipWith (A.+) (A.unit idx) (A.unit idx2))
        sl = A.drop (idx A.+ idx2) array
    in AccSample $
       A.ifThenElse
           (idx2 A.< idx)
           ((A.take idx2 sl) A.++ (A.take wrp array))
           sl

readFromTableA :: (Arrow a, KnownNat l, Numeric aa) => Table aa -> a (A.Exp aa) (AccSample i l aa)
readFromTableA = arr Prelude.. readFromTable 

outA :: (Arrow a) => a b b
outA = arr Prelude.id

countDown :: ArrowCircuit a => Int -> a () Int
countDown x = proc _ -> do
    rec i <- delay x -< i - 1
    outA -< i

countUp :: ArrowCircuit a => a () Int
countUp = proc _ -> do
    rec i <- delay 0 -< i + 1
    outA -< i

osc
    :: forall p a aa l i.
       (Clock p aa, ArrowCircuit a, A.FromIntegral Int aa, Floating aa, KnownNat l, Numeric aa, A.Ord aa, A.ToFloating Int aa)
    => Table aa -> A.Exp aa -> ArrowP a p (A.Exp aa) (AccSample i l aa)
osc table@(Table sz _ _) iphs =
    let ofst = -- why is this offset this value?  sample size / block = window?
            A.fromIntegral (A.the sz) A./
            (A.the $ A.unit (fromIntegral $ natVal (Proxy :: Proxy l)))
    in osc_ iphs ofst >>> readFromTableA table

osc_
    :: forall p a aa.
       ( Clock p aa, ArrowCircuit a, A.RealFrac aa, Num aa, A.Floating aa, A.ToFloating Int aa)
    => A.Exp aa -> A.Exp aa -> ArrowP a p (A.Exp aa) (A.Exp aa)
osc_ phs phsofst =
   let sr = rate (undefined :: p)
   in proc freq -> do
     rec
       let delta = (A.lift sr) A.* freq
           phase = (A.ifThenElse (next A.> (A.constant 1)) (frac next) next)
           phdlt = phase + delta
           win = phs + phsofst
       next <- delay 0 -< (frac (phdlt))
       nextend <-delay 0 -< frac (phdlt + phsofst)
     outA -< phase

-- Generate a sine wave as an accelerate sample
sineWave ::
     forall c i a l.
     ( Clock c a
     , KnownNat l
     , A.Fractional a
     , Fractional a
     , A.Ord a
     , A.FromIntegral Int a
     , A.RealFrac a
     , A.Floating a
     , Floating a
     , A.ToFloating Int a
     )
  => Int
  -> Signal c (A.Acc (A.Array A.DIM1 a)) (AccSample i l a)
sineWave rt =
           let freq = A.constant 440.0
               nrm = A.unit ( A.lift False)
               sz = A.unit (A.lift rt)
               tbl = funToTable A.sin nrm sz
           in proc _ ->  osc tbl (A.the $ A.unit 0) -< freq
