{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import           Control.Monad                       (unless, when)
import           Data.List                           (intersperse)
import           System.Exit                         (exitFailure)
import           System.IO                           (hPutStrLn, stderr)
--
import qualified Sound.MIDI.Message                  as Msg
import qualified Sound.MIDI.Message.Channel          as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice    as VoiceMsg
--
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as Audio
import qualified Sound.JACK.MIDI                     as MIDI
--
import qualified Data.EventList.Absolute.TimeBody    as EventList
--
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Control.Monad.Trans.State.Strict    as MS
import qualified Foreign.C.Error                     as E
import           Foreign.Marshal.Array               (copyArray)
--
import           System.Environment                  (getProgName)
--
import qualified Data.StorableVector.Base            as SVB
--
import           Data.IORef                          (IORef, newIORef,
                                                      readIORef, writeIORef)
--
import           Sound.MIDI.Message.Channel.Voice    (Pitch)
import qualified Sound.MIDI.Message.Channel.Voice    as VoiceMsg
--
import qualified Data.StorableVector                 as SV
import qualified Data.StorableVector.ST.Strict       as SVST
--
import           Control.Monad.ST.Strict             as ST
import           Foreign.Storable                    (Storable)
--
import qualified Control.Monad.Trans.State.Strict    as MS
--
import qualified Data.Map                            as Map
--
import           Control.Monad                       (liftM)
--
import           Debug.Trace                         (trace)


type LTime = Integer -- Long Time
type Time = Int      -- Short Time
type Dur = Int
type Size = Int
type SampleRate = Int

check :: Monad m => Bool -> String -> m () -> m ()
check b msg act = if not b then trace msg $ return ()
                           else act

-- | overwrite all of `xs` into `v`, starting at `start` in `v`
unsafeAddChunkToBuffer :: (Storable a, Num a) =>
   SVST.Vector s a -> Time -> SV.Vector a -> ST s ()
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

-- | Here an "event" is a start time paired with a vector of sound.
-- `size` is the length of the vector that `arrange` creates, which holds
-- all the events.
arrange :: (Storable a, Num a) => Size ->  -- ^ fromIntegral JACK.NFrames
                                  [(Time, SV.Vector a)] ->
                                  SV.Vector a
arrange size evs = SVST.runSTVector $ do
  v <- SVST.new (fromIntegral size) 0
  mapM_ (uncurry $ unsafeAddChunkToBuffer v) evs
  return v

data OscillatorState a = OscillatorState { osciAmp :: a
                                         , osciFreq :: a
                                         , osciPhase :: Int }
type State a = Map.Map Pitch (OscillatorState a)
  -- TODO : Pitch is a (newtype-wrapped) integer. Generalize.

initialState :: State a
initialState = Map.empty

-- | Despite the name, does no IO. Rather, one of its inputs is a list of
-- already finished tones. It adds to that list, if appropriate.
stopTone :: Time
         -> ( Maybe (Time, OscillatorState a)
            , [ (Time, Time, OscillatorState a ) ] )
         -> [ ( Time, Time, OscillatorState a ) ]
stopTone stopTime (mplaying, finished) =
   case mplaying of Just (startTime, osci) ->
                      (startTime, stopTime-startTime, osci) : finished
                    Nothing -> finished

renderTone :: (Storable a, Floating a)
           => Dur
           -> OscillatorState a
           -> ( SV.Vector a
              , OscillatorState a) -- ^ osc state once rendered tone ends
renderTone dur state@(OscillatorState amp freq phase) =
  if dur<0
  then trace ("renderTone: negative duration " ++ show dur) $
       (SV.empty, state)
  else let gain = 0.9999
       in (SV.zipWith (\y k -> y * sin (2 * pi * fromIntegral k * freq))
            (SV.iterateN dur (gain*) amp)
            (SV.iterateN dur (1+) phase),
          OscillatorState (amp*gain^dur) freq (phase+dur))
-- | `phase` here is a sequence of consecutive rising integers.
-- It is the only running variable in the sinewave calculation.
-- Therefore `freq` must be in units other than Hz.

whatDoesThisDo :: forall a. (Storable a, Floating a)
               => Size -- ^ fromIntegral JACK.NFrames
               -> (Maybe (Time, OscillatorState a),
                   [(Time, Dur, OscillatorState a)])
               -> (Maybe (OscillatorState a), [(Time, SV.Vector a)])
whatDoesThisDo size (mplaying, finished) =
  let mplayingNew :: Maybe ((Time, SV.Vector a), OscillatorState a)
      mplayingNew =
        fmap (\(start,s0) ->
                case renderTone (fromIntegral size - start) s0
                of (chunk, s1) -> ((start,chunk), s1))
             mplaying
  in ( fmap snd mplayingNew
     , maybe id (\p -> (fst p :)) mplayingNew
       $ map (\(start, dur, s) -> (start, fst $ renderTone dur s))
             finished)

handleRedundantMidi :: (Floating a)
  => SampleRate
  -> Map.Map Pitch
       (Maybe (Int, OscillatorState a), [(Int, Int, OscillatorState a)])
  -> (Int, VoiceMsg.T)
  -> Map.Map Pitch
       (Maybe (Int, OscillatorState a), [(Int, Int, OscillatorState a)])
handleRedundantMidi rate oscis (time,ev) = case VoiceMsg.explicitNoteOff ev of
  VoiceMsg.NoteOn pitch velocity ->
    Map.insertWith -- A pressed key is pressed again. Should'nt happen.
       (\(newOsci, []) s -> (newOsci, stopTone time s))
       pitch
       ( Just ( time
              , OscillatorState
                (0.2 * 2 ** VoiceMsg.realFromVelocity velocity)
                (VoiceMsg.frequencyFromPitch pitch / fromIntegral rate)
                0 )
       , [] )
       oscis
  VoiceMsg.NoteOff pitch _velocity ->
    Map.adjust -- An unpressed key is released. Shouldn't happen.
       (\s -> (Nothing, stopTone time s))
       pitch
       oscis
  _ -> oscis

processEvents :: (Storable a, Floating a, Monad m) =>
                 Size -> -- ^ fromIntegral JACK.NFrames
                 SampleRate ->
                 [(LTime, VoiceMsg.T)] ->
                 MS.StateT (State a) m [(Int, SV.Vector a)]
processEvents size rate input = do
  oscis0 <- MS.get
  let pendingOscis =
        fmap (whatDoesThisDo size) $
        foldl (handleRedundantMidi rate)
          (fmap (\s -> (Just (0, s), [])) oscis0)
          (map (\(time,ev) -> (fromInteger time, ev)) input)
  MS.put $ Map.mapMaybe fst pendingOscis
  return $ concatMap snd $ Map.elems pendingOscis

run :: (Storable a, Floating a, Monad m) =>
       Size -> -- ^ fromIntegral JACK.NFrames
       SampleRate ->
       [(LTime, VoiceMsg.T)] ->
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
