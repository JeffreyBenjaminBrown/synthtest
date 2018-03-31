{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Main where
import           Control.Monad                       (unless, when)
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Control.Monad.Trans.State.Strict    as MS
import qualified Data.Array.Accelerate               as A
import qualified Data.Array.Accelerate.Array.Sugar   as A
import qualified Data.Array.Accelerate.IO            as A
import qualified Data.Array.Accelerate.IO            as A
import           Data.Coerce                         (coerce)
import qualified Data.EventList.Absolute.TimeBody    as EventList
import           Data.List                           (intersperse)
import qualified Data.StorableVector.Base            as SVB
import qualified Data.Vector.Storable                as DVS
import qualified Foreign.C.Error                     as E
import           Foreign.C.Types                     (CFloat (..))
import           Foreign.Marshal.Array               (copyArray)
import           GHC.TypeLits                        (KnownNat)
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as Audio
import qualified Sound.JACK.MIDI                     as MIDI
import qualified Sound.MIDI.Message                  as Msg
import qualified Sound.MIDI.Message.Channel          as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice    as VoiceMsg
import           System.Environment                  (getProgName)
import           System.Exit                         (exitFailure)
import           System.IO                           (hPutStrLn, stderr)

import           Data.IORef                          (IORef, newIORef,
                                                      readIORef, writeIORef)

import           Sound.MIDI.Message.Channel.Voice    (Pitch)
import qualified Sound.MIDI.Message.Channel.Voice    as VoiceMsg

import qualified Data.StorableVector                 as SV
import qualified Data.StorableVector.ST.Strict       as SVST

import           Control.Monad.ST.Strict             as ST
import           Foreign.Storable                    (Storable)

import qualified Control.Monad.Trans.State.Strict    as MS

import qualified Data.Map                            as Map

import           Control.Monad                       (liftM)
import           Debug.Trace                         (trace)
import           Lib
import           System.IO                           (stderr)


check :: Monad m => Bool -> String -> m () -> m ()
check b msg act = if not b
                  then trace msg $ return ()
                  else act

unsafeAddChunkToBuffer :: (Storable a, Num a, Show a ) =>
   SVST.Vector s a -> Int -> DVS.Vector a -> ST s ()
unsafeAddChunkToBuffer v start xs =
  let go i j =
        if j >= DVS.length xs
        then return ()
        else SVST.unsafeModify v i ( (xs DVS.! j) +) >>
             go (i + 1) (j + 1)
  in check (start >= 0)
           ("start negative: " ++ show (start, DVS.length xs) )
     $ check (start <= SVST.length v)
             ("start too late: " ++ show (start, DVS.length xs) )
     $ check (start + DVS.length xs <= SVST.length v)
             ("end too late: "
              ++ show (start, DVS.length xs,  SVST.length v) )
     $ go start 0
 
type Size = Int 
data OscillatorState a = OscillatorState a a Int

-- the test ignores the Pitch signal; we're just trying to get the sine wave to be written to a buffer
type State a = Map.Map Pitch (OscillatorState a)

arrange ::  (Storable a, Num a, Show a) =>
  Size -> [(Int, DVS.Vector a)] -> SV.Vector CFloat
arrange size evs =
  SVST.runSTVector $ 
         do v <- SVST.new (fromIntegral size) 0
            mapM_ (uncurry $ unsafeAddChunkToBuffer v) evs
            return $ coerce v

instance Clock AudRate Float where
  rate _ = A.constant 44100

-- renderToneNrm ::
--    (Storable a, Floating a, A.RealFrac a, A.FromIntegral Int a, Storable a, Floating a, Monad m, Num a, A.Elt a) =>
--    Int -> OscillatorState a ->
--    m [(Int, DVS.Vector a)]
-- renderToneNrm dur state@(OscillatorState amp freq phase) =
--        let gain = 0.9999
--        in   return [( 1, (DVS.zipWith (\y k -> y * sin (2*pi*fromIntegral k * freq))
--                (DVS.iterateN dur (gain*) amp)
--                (DVS.iterateN dur (1+) phase)))]

gen ::
  Signal (AudRate) (A.Acc (A.Array A.DIM1 Float)) (AccSample LLVM 44100 Float)
  -> DVS.Vector Float
gen sig = runToStorable sig (A.fromList (A.Z A.:. 1) [0])

renderTone :: ( A.RealFrac a, A.FromIntegral Int a, Storable a
              , Floating a, Monad m, Num a, A.Elt a ) =>
  Int -> MS.StateT (State a) m [(Int, DVS.Vector a)]
renderTone rt = let out = coerce $ gen $ sineWave rt
  in (trace $ "renderTone: " ++ show out) $
     return $ map (\v -> ( 0, out)) [0..]

run :: (A.RealFrac a, A.FromIntegral Int a, Storable a, Floating a, Monad m)
    => Int -> Int -> MS.StateT (State a) m  (SV.Vector CFloat)
run size rt = liftM (arrange size) $ renderTone rt
  -- renderToneNrm 2 (OscillatorState 1 440 1) -- renderTone rt

mainWait client name = JACK.withActivation client $
                       Trans.lift $
                       do putStrLn $ "started " ++ name ++ "..."
                          JACK.waitForBreak

initialState :: State a
initialState = Map.empty

main :: IO ()
main = do
  name <- getProgName
  stateRef <- newIORef initialState
  JACK.handleExceptions
    $ JACK.withClientDefault name
    $ \client ->
        JACK.withPort client "output"
        $ \output -> JACK.withProcess client (process client stateRef output)
                     $ mainWait client name

runStateOnIORef :: IORef s -> MS.State s a -> IO a
runStateOnIORef ref m = do (a,state) <- fmap (MS.runState m) $ readIORef ref
                           writeIORef ref state
                           return a

intFromNFrames :: Integral i => JACK.NFrames -> i
intFromNFrames (JACK.NFrames n) = fromIntegral n

process :: JACK.Client
        -> IORef (State Audio.Sample)
        -> Audio.Port JACK.Output
        -> JACK.NFrames
        -> Sync.ExceptionalT E.Errno IO ()
process client stateRef output nframes = do
  Trans.lift $
    do rt <- JACK.getSampleRate client
       -- putStrLn $ "rate " ++  show rt
       -- putStrLn $ "nFrames " ++ (show $ intFromNFrames nframes)
       outArr <- Audio.getBufferPtr output nframes
       block <- runStateOnIORef stateRef $ run (intFromNFrames nframes) rt
       -- putStrLn $ show block
       SVB.withStartPtr block
         $ \src len -> copyArray outArr src len
