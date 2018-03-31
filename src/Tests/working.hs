
module Main where
import           Control.Monad                       (unless, when)
import           Data.List                           (intersperse)
import           System.Exit                         (exitFailure)
import           System.IO                           (hPutStrLn, stderr)

import qualified Sound.MIDI.Message                  as Msg
import qualified Sound.MIDI.Message.Channel          as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice    as VoiceMsg

import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as Audio
import qualified Sound.JACK.MIDI                     as MIDI

import qualified Data.EventList.Absolute.TimeBody    as EventList

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Control.Monad.Trans.State.Strict    as MS
import qualified Foreign.C.Error                     as E
import           Foreign.Marshal.Array               (copyArray)

import           System.Environment                  (getProgName)

import qualified Data.StorableVector.Base            as SVB

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


type Time = Integer
type Size = Int
type SampleRate = Int

check :: Monad m => Bool -> String -> m () -> m ()
check b msg act = if not b
                  then trace msg $ return ()
                  else act

unsafeAddChunkToBuffer :: (Storable a, Num a) =>
   SVST.Vector s a -> Int -> SV.Vector a -> ST s ()
unsafeAddChunkToBuffer v start xs =
   let go i j = if j >= SV.length xs
                then return ()
                else SVST.unsafeModify v i (SV.index xs j +) >>
                     go (i + 1) (j + 1)
   in check (start>=0)
               ("start negative: " ++ show (start, SV.length xs)) $
      check (start <= SVST.length v)
               ("start too late: " ++ show (start, SV.length xs)) $
      check (start+SV.length xs <= SVST.length v)
               ("end too late: " ++ show (start, SV.length xs)) $
      go start 0

arrange :: (Storable a, Num a) => Size ->
                                  [(Int, SV.Vector a)] ->
                                  SV.Vector a
arrange size evs = SVST.runSTVector $ do
  v <- SVST.new (fromIntegral size) 0
  mapM_ (uncurry $ unsafeAddChunkToBuffer v) evs
  return v

data OscillatorState a = OscillatorState a a Int
type State a = Map.Map Pitch (OscillatorState a)

initialState :: State a
initialState = Map.empty

stopTone :: Int
         -> (Maybe (Int, OscillatorState a),
             [(Int, Int, OscillatorState a)])
         -> [(Int, Int, OscillatorState a)]
stopTone stopTime (mplaying, finished) =
   case mplaying of Just (startTime, osci) ->
                      (startTime, stopTime-startTime, osci) : finished
                    Nothing -> finished

renderTone :: (Storable a, Floating a)
           => Int
           -> OscillatorState a
           -> (SV.Vector a, OscillatorState a)
renderTone dur state@(OscillatorState amp freq phase) =
  if dur<0
  then trace ("renderTone: negative duration " ++ show dur) $
       (SV.empty, state)
  else let gain = 0.9999
       in (SV.zipWith (\y k -> y * sin (2*pi*fromIntegral k * freq))
            (SV.iterateN dur (gain*) amp)
            (SV.iterateN dur (1+) phase),
          OscillatorState (amp*gain^dur) freq (phase+dur))

processEvents :: (Storable a, Floating a, Monad m) =>
                 Size ->
                 SampleRate ->
                 [(Time, VoiceMsg.T)] ->
                 MS.StateT (State a) m [(Int, SV.Vector a)]
processEvents size rate input = do
  oscis0 <- MS.get
  let qq1 (mplaying, finished) =
        let mplayingNew =
              fmap (\(start,s0) ->
                      case renderTone (fromIntegral size - start) s0
                      of (chunk, s1) -> ((start,chunk), s1))
              mplaying
        in ( fmap snd mplayingNew
           , maybe id (\p -> (fst p :)) mplayingNew $
             map (\(start, dur, s) -> (start, fst $ renderTone dur s))
                 finished)
      qq2 oscis (time,ev) = case VoiceMsg.explicitNoteOff ev of
        VoiceMsg.NoteOn pitch velocity ->
          Map.insertWith
             (\(newOsci, []) s ->
                {-
                A key may be pressed that was already pressed.
                This should not happen, but we must be prepared for it.
                Thus we call stopTone.
                -}
                (newOsci, stopTone time s))
             pitch
             (Just (time,
                 OscillatorState
                    (0.2 * 2 ** VoiceMsg.realFromVelocity velocity)
                    (VoiceMsg.frequencyFromPitch pitch /
                     fromIntegral rate)
                    0),
              [])
             oscis
        VoiceMsg.NoteOff pitch _velocity ->
          Map.adjust
             (\s ->
                {-
                A key may be released that was not pressed.
                This should not happen, but we must be prepared for it.
                Thus stopTone also handles that case.
                -}
                (Nothing, stopTone time s))
             pitch
             oscis
        _ -> oscis
      pendingOscis =
        fmap qq1 $
        foldl qq2
           (fmap (\s -> (Just (0, s), [])) oscis0)
           (map (\(time,ev) -> (fromInteger time, ev)) input)
  MS.put (Map.mapMaybe fst pendingOscis)
  return (concatMap snd $ Map.elems pendingOscis)

run :: (Storable a, Floating a, Monad m) =>
       Size ->
       SampleRate ->
       [(Time, VoiceMsg.T)] ->
       MS.StateT (State a) m (SV.Vector a)
run size rate input = 
   liftM (arrange size) $ processEvents size rate input

mainWait client name = JACK.withActivation client $ Trans.lift $ do
  putStrLn $ "started " ++ name ++ "..."
  JACK.waitForBreak

main :: IO ()
main = do
  name <- getProgName
  stateRef <- newIORef initialState
  JACK.handleExceptions $
    JACK.withClientDefault name $ \client ->
    JACK.withPort client "input" $ \input ->
    JACK.withPort client "output" $ \output ->
    JACK.withProcess client (process client stateRef input output) $
      mainWait client name

checkVoiceMsg :: Msg.T -> Maybe VoiceMsg.T
checkVoiceMsg ev = case ev of
  Msg.Channel (ChannelMsg.Cons _chan (ChannelMsg.Voice dat))
    -> Just dat
  _ -> Nothing

intFromNFrames :: Integral i => JACK.NFrames -> i
intFromNFrames (JACK.NFrames n) = fromIntegral n

runStateOnIORef :: IORef s -> MS.State s a -> IO a
runStateOnIORef ref m = do
    (a, state) <- fmap (MS.runState m) $ readIORef ref
    writeIORef ref state
    return a

process :: JACK.Client ->
           IORef (State Audio.Sample) ->
           MIDI.Port JACK.Input ->
           Audio.Port JACK.Output ->
           JACK.NFrames ->
           Sync.ExceptionalT E.Errno IO ()
process client stateRef input output nframes = do
  evs <- MIDI.readEventsFromPort input nframes
  Trans.lift $ do
    rate <- JACK.getSampleRate client
    outArr <- Audio.getBufferPtr output nframes
    block <-
      runStateOnIORef stateRef $
      run (intFromNFrames nframes) rate $
      EventList.toPairList $
      EventList.mapMaybe checkVoiceMsg $
      EventList.mapTime intFromNFrames evs :: IO (SVB.Vector Audio.Sample)
    SVB.withStartPtr block $ \src len -> copyArray outArr src len
