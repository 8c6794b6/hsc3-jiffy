-- | Compatibility functions for hsc3 and hsc3-jiffy

module Sound.SC3.Jiffy.Compat
  ( Synthdef(..)
  , synthdef
  , defaultSynthdef
  , defaultSampler
  , synthdefGraph
  , synthdefParam
  , synthdef_to_graphdef
  , synthdefData
  , synthdefWrite
  , synthdefWrite_dir
  , synthstat
  , synthstat_ln
  , synthstat_wr
  , synthstat_concise
  , d_recv
  ) where

-- bytestring
import qualified Data.ByteString.Lazy as L

-- filepath
import System.FilePath ((</>), (<.>))

-- hosc
import Sound.OSC (Message, sendMessage)

-- hsc3
import Sound.SC3
  ( DoneAction(..), Envelope_Curve(..), Loop(..), Rate(..)
  , envLinen, envASR, d_recv_bytes, s_new)
import Sound.SC3.Server.Command.Generic (withCM)
import Sound.SC3.Server.Transport.Monad (Audible(..))
import Sound.SC3.UGen.Graph (U_Graph(..), U_Node(..), ug_stat_ln)
import qualified Sound.SC3.Server.Graphdef as Graphdef

-- Internal
import Sound.SC3.Jiffy.Encode
import Sound.SC3.UGen.Jiffy.Bindings
import Sound.SC3.UGen.Jiffy.Builder


--
-- Synthdef
--

-- | A named unit generator graph.
data Synthdef = Synthdef { synthdefName :: String
                         , synthdefUGen :: UGen }
  deriving (Eq)

instance Show Synthdef where
  show (Synthdef name _) = "<synthdef " ++ name ++ ">"

instance Audible Synthdef where
  play_at (nid,aa,gid,params) sd =
    let dr = d_recv sd
        sn = s_new (synthdefName sd) nid aa gid params
    in  sendMessage (withCM dr sn)

-- | Alias for 'Synthdef'.
synthdef :: String -> UGen -> Synthdef
synthdef = Synthdef

-- | The SC3 /default/ instrement.
defaultSynthdef :: Synthdef
defaultSynthdef = synthdef "default" default_ugen_graph

-- | The SC3 /default/ sample (buffer) playback instrument.
defaultSampler :: Bool -> Synthdef
defaultSampler use_gate =
  let nm = "default-sampler-" ++ sfx
      sfx | use_gate = "gate"
          | otherwise = "fixed"
  in  synthdef nm (default_sampler_ugen_graph use_gate)

synthdefGraph :: Synthdef -> U_Graph
synthdefGraph = ugen_to_graph . synthdefUGen

synthdefParam :: Synthdef -> [String]
synthdefParam = map u_node_k_name . ug_controls . synthdefGraph

synthdef_to_graphdef :: Synthdef -> Graphdef.Graphdef
synthdef_to_graphdef (Synthdef n u) = ugen_to_graphdef n u

synthdefData :: Synthdef -> L.ByteString
synthdefData = encode_graphdef . synthdef_to_graphdef

synthdefWrite :: FilePath -> Synthdef -> IO ()
synthdefWrite path = L.writeFile path . synthdefData

synthdefWrite_dir :: FilePath -> Synthdef -> IO ()
synthdefWrite_dir dir sd@(Synthdef name _) =
  synthdefWrite (dir </> name <.> "scsyndef") sd

synthstat_ln :: UGen -> [String]
synthstat_ln = ug_stat_ln . ugen_to_graph

synthstat :: UGen -> String
synthstat = unlines . synthstat_ln

synthstat_wr :: UGen -> IO ()
synthstat_wr = putStrLn . synthstat

synthstat_concise :: UGen -> String
synthstat_concise ug =
  let lns = synthstat_ln ug
  in  unlines (zipWith const lns (tail lns))


--
-- OSC command for scsynth server
--

d_recv :: Synthdef -> Message
d_recv = d_recv_bytes . synthdefData


--
-- UGen graphs
--

default_ugen_graph :: UGen
default_ugen_graph =
  let f = control KR "freq" 440
      a = control KR "amp" 0.1
      p = control KR "pan" 0
      g = control KR "gate" 1
      o = control KR "out" 0
      e = linen g 0.01 0.7 0.3 RemoveSynth
      f3 = mce [f, f + rand (-0.4) 0, f + rand 0 0.4]
      l = xLine KR (rand 4000 5000) (rand 2500 3200) 1 DoNothing
      z = lpf (mix (varSaw AR f3 0 0.3 * 0.3)) l * e
  in  out o (pan2 z p a)

default_sampler_ugen_graph :: Bool -> UGen
default_sampler_ugen_graph use_gate =
  let b = control KR "bufnum" 0
      l = control KR "pan" 0
      a = control KR "amp" 0.1
      r = control KR "rate" 1
      m = control KR "rdelay" 0
      v = control KR "ramplitude" 0
      w = control KR "attack" 0
      y = control KR "decay" 0.5
      r' = bufRateScale KR b * r
      p = playBuf 1 AR b r' 1 0 NoLoop RemoveSynth
      e = if use_gate
          then let g = control KR "gate" 1
               in envGen KR g 1 0 1 RemoveSynth (envASR w 1 y EnvSin)
          else let s = control KR "sustain" 1
               in envGen KR 1 1 0 1 RemoveSynth (envLinen w s y 1)
      d = delayC (p * e) m (rand 0 m)
  in out 0 (pan2 d l (a + rand 0 v))
