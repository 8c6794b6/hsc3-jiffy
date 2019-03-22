-- | Tests comparing synthdef graphs built with hsc3 functions and
-- hsc3-jiffy functions.
module Hsc3Tests
  ( hsc3Tests
  ) where

-- base
import Data.Foldable (Foldable(..), find)

-- hspec
import Test.Hspec

-- hsc3
import Sound.SC3
  ( Envelope_Curve(..), Rate(..), Loop(..), DoneAction(..)
  , UnaryOp(..), Warp(..)
  , envelope )
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)
import Sound.SC3.UGen.Graph
  ( U_Graph(..), U_Node(..), From_Port(..), ug_find_node)
import qualified Sound.SC3 as S
import qualified Sound.SC3.UGen.Graph as SG
import qualified Sound.SC3.Server.Graphdef as SSG

-- Internal
import Sound.SC3.Jiffy.Encode (encode_graphdef)
import Sound.SC3.UGen.Jiffy


--
-- Equality of U_Graph
--

eq_U_Graph :: U_Graph -> U_Graph -> Bool
eq_U_Graph g1@(U_Graph _ cs1 ks1 _) g2@(U_Graph _ cs2 ks2 _) =
  eq_U_Node_constants cs1 cs2 &&
  eq_U_Node_controls ks1 ks2 &&
  eq_U_Node_ugens g1 g2

eq_U_Node_constants :: [U_Node] -> [U_Node] -> Bool
eq_U_Node_constants xs ys
  | length xs /= length ys = False
  | otherwise = all findConstant xs
  where
    findConstant x = maybe False (const True) (find (eq_U_Node_C x) ys)

eq_U_Node_C :: U_Node -> U_Node -> Bool
eq_U_Node_C u1 u2
  | U_Node_C _ v1 <- u1, U_Node_C _ v2 <- u2 = v1 == v2
  | otherwise = False

eq_U_Node_controls :: [U_Node] -> [U_Node] -> Bool
eq_U_Node_controls xs ys
  | length xs /= length ys = False
  | otherwise              = all findControl xs
  where
    findControl x = maybe False (const True) (find (eq_U_Node_K x) ys)

eq_U_Node_K :: U_Node -> U_Node -> Bool
eq_U_Node_K x y
  | U_Node_K {u_node_k_rate=r1
             ,u_node_k_name=n1
             ,u_node_k_default=d1
             ,u_node_k_type=k1} <- x
  , U_Node_K {u_node_k_rate=r2
             ,u_node_k_name=n2
             ,u_node_k_default=d2
             ,u_node_k_type=k2} <- y
  = r1 == r2 && n1 == n2 && d1 == d2 && k1 == k2
  | otherwise = False

eq_U_Node_ugens :: U_Graph -> U_Graph -> Bool
eq_U_Node_ugens g1@(U_Graph _ _ _ us1) g2@(U_Graph _ _ _ us2)
  | null us1 && null us2 = True
  | otherwise            = all (\u -> any (eq_U_Node u) us2) us1
  where
    eq_U_Node u1 u2
      | U_Node_U {u_node_u_rate=r1
                 ,u_node_u_name=n1
                 ,u_node_u_inputs=i1
                 ,u_node_u_outputs=o1
                 ,u_node_u_special=s1} <- u1
      , U_Node_U {u_node_u_rate=r2
                 ,u_node_u_name=n2
                 ,u_node_u_inputs=i2
                 ,u_node_u_outputs=o2
                 ,u_node_u_special=s2} <- u2
      = r1 == r2 && n1 == n2 && eq_From_Ports i1 i2 &&
        o1 == o2 && s1 == s2
      | U_Node_K {} <- u1, U_Node_K {} <- u2
      = eq_U_Node_K u1 u2
      | U_Node_C {} <- u1, U_Node_C {} <- u2
      = eq_U_Node_C u1 u2
      | otherwise = False
    eq_From_Ports ps1 ps2 = and (zipWith eq_From_Port ps1 ps2)
    eq_From_Port p1 p2 =
      case ( ug_find_node g1 (from_port_nid p1)
           , ug_find_node g2 (from_port_nid p2) ) of
        (Just u1, Just u2) -> eq_U_Node u1 u2
        _                  -> False

--
-- Functions to make 'Spec'
--

same_blob :: UGen -> S.UGen -> Spec
same_blob j h =
  let gj = encode_graphdef (ugen_to_graphdef "tmp" j)
      gh = SSG.encode_graphdef
             (graph_to_graphdef "tmp" (SG.ugen_to_graph h))
  in  it "is_same_blob_data" (gj `shouldBe` gh)

same_graph :: UGen -> S.UGen -> Spec
same_graph j h = do
  let gj = ugen_to_graph j
      gh = SG.ugen_to_graph h
      gdj = ugen_to_graphdef "g" j
      gj' = graph_to_graphdef "g" gj
  it "is_same_U_Graph" (gj `shouldSatisfy` eq_U_Graph gh)
  it "is_same_Graphdef_via_U_Graph" (gdj `shouldBe` gj')

same_graph_and_blob :: UGen -> S.UGen -> Spec
same_graph_and_blob j h = same_graph j h >> same_blob j h

--
-- Specs
--

simple_graph :: Spec
simple_graph = describe "simple" $ do
  let j = out 0 (sinOsc AR (hpf (whiteNoise AR) 0) 0)
      h = S.out 0 (S.sinOsc AR (S.hpf (S.whiteNoise 'a' AR) 0) 0)
  same_graph_and_blob j h

mix_mce_graph :: Spec
mix_mce_graph = do
  let j0 = let f1 = control KR "f1" 0
           in out 0 (pan2 (mix (sinOsc AR (mce [f1,0]) 0)) 0 0)
      h0 = let f1 = S.control KR "f1" 0
           in  S.out 0 (S.pan2 (S.mix (S.sinOsc AR (S.mce [f1,0]) 0)) 0 0)
  describe "mix_mce" $ same_graph_and_blob j0 h0
  let j1 = do [s0,s1] <- mceChannels (sinOsc AR (mce2 440 550) 0)
              let n = lfNoise0 KR 4
              out 0 (balance2 s0 s1 n 0.3)
      h1 = let [s0,s1] = S.mceChannels (S.sinOsc AR (S.mce2 440 550) 0)
               n = S.lfNoise0 'a' KR 4
           in S.out 0 (S.balance2 s0 s1 n 0.3)
  describe "mceChannels" $ same_graph j1 h1
  let j2 = let i = in' 2 AR numOutputBuses
               c = mceChannel
               x = mouseX KR 0 1 Linear 0.1
               y = mouseY KR 0 1 Linear 0.1
           in  out 0 (freeVerb2 (c 0 i) (c 1 i) y x 0.5)
      h2 = let i = S.in' 2 AR S.numOutputBuses
               c = S.mceChannel
               x = S.mouseX KR 0 1 Linear 0.1
               y = S.mouseY KR 0 1 Linear 0.1
           in  S.out 0 (S.freeVerb2 (c 0 i) (c 1 i) y x 0.5)
  describe "mceChannel" $ same_graph j2 h2
  let j3 = let p1 = mce [440,660,990,1320]
               p2 = mce [0,1]
               o = sinOsc AR p1 p2
           in  out 0 (mix o * 0.2)
      h3 = let p1 = S.mce [440,660,990,1320]
               p2 = S.mce [0,1]
               o = S.sinOsc AR p1 p2
           in  S.out 0 (S.mix o * 0.2)
  describe "mceExtend" $ same_graph j3 h3

nondet_graph :: Spec
nondet_graph = describe "nondet" $ do
  let j = do w1 <- share (whiteNoise AR)
             let w2 = whiteNoise AR
             out 0 (mce [w1-w1, w2-w2])
      h = let w1 = S.whiteNoise 'a' AR
              wx x = S.whiteNoise x AR
          in  S.out 0 (S.mce [w1-w1, wx 'b' - wx 'c'])
  same_graph_and_blob j h

mrg_graph :: Spec
mrg_graph = describe "mrg" $ do
  -- Using 'share' to control the order of constant values, storing 0
  -- before 1.
  let j = do s1 <- share (sinOsc AR 0 0)
             _ <- out 1 s1
             out 0 s1
      h = let s1 = S.sinOsc AR 0 0
          in  S.mrg [S.out 0 s1, S.out 1 s1]
  same_graph_and_blob j h

enum_graph :: Spec
enum_graph = do
  let j0 = out 0 (playBuf 2 AR 0 0 0 0 NoLoop DoNothing)
      h0 = S.out 0 (S.playBuf 2 AR 0 0 0 0 NoLoop DoNothing)
      j1 = out 0 (mouseX KR 0 0 Linear 0)
      h1 = S.out 0 (S.mouseX KR 0 0 Linear 0)
  describe "enum_loop_doneaction" $ same_graph_and_blob j0 h0
  describe "enum_warp" $ same_graph_and_blob  j1 h1

controls_graph :: Spec
controls_graph = describe "controls_with_various_rates" $ do
  let j0 = let c1 = tr_control "t_amp" 0
               c2 = control KR "freq" 0
               c3 = control AR "amp" 0
               c4 = control IR "dur" 0
               e = decay c1 c4
               s = sinOsc AR c2 0 * e * c3
           in  out 0 (mce [s,s])
      h0 = let c1 = S.tr_control "t_amp" 0
               c2 = S.control KR "freq" 0
               c3 = S.control AR "amp" 0
               c4 = S.control IR "dur" 0
               e = S.decay c1 c4
               s = S.sinOsc AR c2 0 * e * c3
           in  S.out 0 (S.mce [s,s])
  same_graph_and_blob j0 h0

unary_op_graph :: Spec
unary_op_graph = describe "constant_folding_with_unary_op" $ do
  let j0 = out 0 (saw AR (S.midiCPS (- (constant inf))))
      h0 = S.out 0 (S.saw AR (S.midiCPS (- (S.constant inf))))
      inf :: S.Sample
      inf = 1/0
  same_graph_and_blob j0 h0

optimize_graph :: Spec
optimize_graph = do
  let j0 = out 1 (sinOsc AR 1 1 * decay (tr_control "t" 1) 1 + 1)
      h0 = S.out 1 (S.sinOsc AR 1 1 * S.decay (S.tr_control "t" 1) 1 + 1)
  describe "optimizing_muladd" $ same_graph_and_blob j0 h0
  let j1 = let s = sinOsc AR 0 0 in out 0 (s+s+s)
      h1 = let s = S.sinOsc AR 0 0 in S.out 0 (s+s+s)
  describe "optimizing_sum3" $ same_graph_and_blob j1 h1

demand_graph :: Spec
demand_graph = describe "demand" $ do
  let j0 = do p <- share (dseq 3 (mce [0,3,5,7,5,3]))
              let t = impulse KR 2 0
                  d = demand t 0 (mce2 p p)
                  o = sinOsc AR (midiCPS (d+60)) 0
              out 0 (o*0.1)
      h0 = let t = S.impulse KR 2 0
               p = S.dseq 'a' 3 (S.mce [0,3,5,7,5,3])
               d = S.demand t 0 (S.mce2 p p)
               o = S.sinOsc AR (midiCPS (d+60)) 0
           in  S.out 0 (o*0.1)
  same_graph j0 h0

envelope_graph :: Spec
envelope_graph = describe "envelope" $ do
  let j0 = let e = envGen KR 1 0.3 0 1 DoNothing c
               c = envelope [0,1,0] [0.3,0.7] [EnvCub,EnvCub]
               o = sinOsc AR 440 0 * e
           in  out 0 o
      h0 = let e = S.envGen KR 1 0.3 0 1 DoNothing c
               c = envelope [0,1,0] [0.3,0.7] [EnvCub,EnvCub]
               o = S.sinOsc AR 440 0 * e
           in  S.out 0 o
  same_graph j0 h0

pv_graph :: Spec
pv_graph = describe "pv" $ do
  let j0 = let buf = localBuf 1 2048
               f = fft' buf (whiteNoise AR)
               p0 = control KR "p0" 0.1
               c = pv_BrickWall f (sinOsc KR p0 0 * 0.75)
           in  out 0 (ifft' c * 0.1)
      h0 = let buf = S.localBuf 'a' 1 2048
               f = S.fft' buf (S.whiteNoise 'b' AR)
               p0 = S.control KR "p0" 0.1
               c = S.pv_BrickWall f (S.sinOsc KR p0 0 * 0.75)
           in  S.out 0 (S.ifft' c * 0.1)
  same_graph j0 h0

handwritten_graph :: Spec
handwritten_graph =
  describe "handwritten" $ do

    describe "asLocalBuf" $ do
      let j0 = do
            b <- share (asLocalBuf [-1,-1+(2/255)..1])
            _ <- clearBuf b
            let o = osc AR b 110 0
            out 0 (o*0.1)
          h0 =
            let b = S.asLocalBuf 'a' [-1,-1+(2/255)..1]
                o = S.osc AR (S.clearBuf b) 110 0
            in  S.out 0 (o*0.1)
      same_graph j0 h0

    describe "changed" $ do
      let j0 = do
            n <- share (lfdNoise0 KR 1)
            let s = sinOsc AR (exprange 110 3300 n) 0
                e = decay (changed n 0.01) 1
            out 0 (s*e*0.3)
          h0 =
            let n = S.lfdNoise0 'a' KR 1
                s = S.sinOsc AR (S.exprange 110 3300 n) 0
                e = S.decay (S.changed n 0.01) 1
            in  S.out 0 (s*e*0.3)
      same_graph j0 h0

    describe "dinf" $ do
      it "should_be_infinity" $
        case runUGen dinf of
          MCEU (NConstant x) -> x `shouldSatisfy` isInfinite
          _ -> expectationFailure "not a constant"

    describe "dwrand" $ do
      let j0 = let n = dwrand 32 (mce [0.5,0.3,0.2]) (mce [1,3,7])
                   x = mouseX KR 1 400 Exponential 0.1
                   t = impulse KR x 0
                   f = demand t 0 n * 30 + 340
               in  out 0 (sinOsc AR f 0 * 0.1)
          h0 = let n = S.dwrand 'a' 32 (S.mce [0.5,0.3,0.2]) (S.mce [1,3,7])
                   x = S.mouseX KR 1 400 Exponential 0.1
                   t = S.impulse KR x 0
                   f = S.demand t 0 n * 30 + 340
               in  S.out 0 (S.sinOsc AR f 0 * 0.1)
      same_graph j0 h0

    describe "exprange" $ do
      let j0 = out 0 (exprange 0.1 8 (sinOsc KR 4 0))
          h0 = S.out 0 (S.exprange 0.1 8 (S.sinOsc KR 4 0))
      same_graph j0 h0

    describe "fftTrigger" $ do
      let j0 = out 0 (fftTrigger (localBuf 1 512) 0.5 0)
          h0 = S.out 0 (S.fftTrigger (S.localBuf 'a' 1 512) 0.5 0)
      same_graph j0 h0

    describe "klangSpec" $ do
      let j0 = let a = klangSpec [440,880,1320] [0.5,0.4,0.3] [0,0,0]
               in  out 0 (klang AR 1 0 a * 0.2)
          h0 = let a = S.klangSpec [440,880,1320] [0.5,0.4,0.3] [0,0,0]
               in  S.out 0 (S.klang AR 1 0 a * 0.2)
      same_graph j0 h0

    describe "klankSpec" $ do
      let j0 = let a = klankSpec [330,440,770] [0.5,0.4,0.3] [1,1.2,0.8]
               in  out 0 (klank (impulse AR 1 0) 1 0 1 a)
          h0 = let a = S.klankSpec [330,440,770] [0.5,0.4,0.3] [1,1.2,0.8]
               in  S.out 0 (S.klank (S.impulse AR 1 0) 1 0 1 a)
      same_graph j0 h0

    describe "linLin" $ do
      let j0 = out 0 (linLin (sinOsc AR 440 0) (-1) 1 0.5 2)
          h0 = S.out 0 (S.linLin (S.sinOsc AR 440 0) (-1) 1 0.5 2)
      same_graph j0 h0

    describe "packFFT" $ do
      let j0 =
            let s = packFFTSpec [1,1,1,1] [0,0,0,0]
            in  out 0 (packFFT (localBuf 4 1) 4 0 3 0 s)
          h0 =
            let s = S.packFFTSpec [1,1,1,1] [0,0,0,0]
            in  S.out 0 (S.packFFT (S.localBuf 'a' 4 1) 4 0 3 0 s)
      same_graph j0 h0

    describe "poll" $ do
      let j0 = let t = impulse KR 10 0
                   l = line KR 0 1 1 RemoveSynth
               in  poll t l 0 "polling ..."
          h0 = let t = S.impulse KR 10 0
                   l = S.line KR 0 1 1 RemoveSynth
               in  S.poll t l 0 (S.label "polling ...")
      same_graph j0 h0

    describe "pv_HainsworthFoote" $ do
      let j0 = do
            s <- share (lfSaw AR (lfNoise0 KR 1 * 90 + 400) 0 * 0.5)
            let b = localBuf 2048 1
                f = fft' b s
                d = pv_HainsworthFoote f 1 0 0.9 0.5
                t = sinOsc AR 440 0 * decay (d*0.1) 0.1
            out 0 (mce2 (s*0.2) t)
          h0 =
            let s = S.lfSaw AR (S.lfNoise0 'a' KR 1 * 90 + 400) 0 * 0.5
                b = S.localBuf 'b' 2048 1
                f = S.fft' b s
                d = S.pv_HainsworthFoote f 1 0 0.9 0.5
                t = S.sinOsc AR 440 0 * S.decay (d*0.1) 0.1
            in  S.out 0 (S.mce2 (s*0.2) t)
      same_graph j0 h0

    describe "pvcollect" $ do
      let j0 = let sf = sinOsc AR 440 0
                   c1 = fft' 10 sf
                   f m p _ = (m+delayN m 1 1,p)
                   c2 = pvcollect c1 4 f 0 5 0
               in  out 0 (ifft' c2)
          h0 = let sf = S.sinOsc AR 440 0
                   c1 = S.fft' 10 sf
                   f m p _ = (m+S.delayN m 1 1,p)
                   c2 = S.pvcollect c1 4 f 0 5 0
               in  S.out 0 (S.ifft' c2)
      same_graph j0 h0

    describe "sendReply" $ do
      let j0 = do
            s0 <- share (lfdNoise0 KR 5)
            s1 <- share (lfdNoise0 KR 5)
            let o = sinOsc AR (s0 * 200 + 500) 0 * s1 * 0.1
            _ <- sendReply s0 0 "/send-reply" [s0,s1]
            out 0 o
          h0 =
            let s0 = S.lfdNoise0 'a' KR 5
                s1 = S.lfdNoise0 'b' KR 5
                o =  S.sinOsc AR (s0 * 200 + 500) 0 * s1 * 0.1
            in  S.mrg [S.out 0 o, S.sendReply s0 0 "/send-reply" [s0,s1]]
      same_graph j0 h0

    describe "soundIn" $ do
      let j0 = out 0 (soundIn 0)
          h0 = S.out 0 (S.soundIn 0)
      same_graph j0 h0
      let j1 = out 0 (soundIn (mce2 0 1))
          h1 = S.out 0 (S.soundIn (S.mce2 0 1))
      same_graph j1 h1
      let j2 = out 0 (soundIn (mce2 2 4))
          h2 = S.out 0 (S.soundIn (S.mce2 2 4))
      same_graph j2 h2

    describe "tap" $ do
      let j0 = out 0 (tap 1 0 (mce2 0.5 0.8))
          h0 = S.out 0 (S.tap 1 0 (S.mce2 0.5 0.8))
      same_graph j0 h0

    describe "tChoose" $ do
      let j0 = let a = [sinOsc AR 440 0
                       ,saw AR 440
                       ,pulse AR 440 0.5]
                   o = tChoose (dust KR 2) a
               in  out 0 o
          h0 = let a = [S.sinOsc AR 440 0
                       ,S.saw AR 440
                       ,S.pulse AR 440 0.5]
                   o = S.tChoose 'a' (S.dust 'b' KR 2) (S.mce a)
               in  S.out 0 o
      same_graph j0 h0

    describe "tWChoose" $ do
      let j0 = let a = [sinOsc AR 440 0
                       ,saw AR 220
                       ,pulse AR 110 0.1]
                   w = [0.5,0.35,0.15]
                   o = tWChoose (impulse KR 4 0) a w 0
               in  out 0 o
          h0 = let a = [S.sinOsc AR 440 0
                       ,S.saw AR 220
                       ,S.pulse AR 110 0.1]
                   w = [0.5,0.35,0.15]
                   t = S.impulse KR 4 0
                   o = S.tWChoose 'a' t (S.mce a) (S.mce w) 0
               in  S.out 0 o
      same_graph j0 h0

    describe "unpackFFT" $ do
      let j0 = do
            b <- share (localBuf 1024 1)
            f <- share (lfdNoise3 KR (lfNoise0 KR 1 * 40 + 60) * 700 + 800)
            let s = sinOsc AR f 0
                c0 = fft' b s
                (ms0,ps0) = unzip (unpackFFT c0 1 0 0)
                (ms1,ps1) = (map sqrt ms0, map sqrt ps0)
                c1 = packFFT c0 1 0 0 0 (packFFTSpec ms1 ps1)
            out 0 (ifft' c1 * 0.1)
          h0 =
            let b = S.localBuf 'a' 1024 1
                f0 = (S.lfNoise0 'c' KR 1 * 40 + 60)
                f = S.lfdNoise3 'b' KR f0 * 700 + 800
                s = S.sinOsc AR f 0
                c0 = S.fft' b s
                mags0 = S.unpackFFT c0 1 0 0 0
                phss0 = S.unpackFFT c0 1 0 0 1
                mags1 = map sqrt mags0
                phss1 = map sqrt phss0
                c1 = S.packFFT c0 1 0 0 0 (S.packFFTSpec mags1 phss1)
            in  S.out 0 (S.ifft' c1 * 0.1)
      same_graph j0 h0

    describe "wrapOut" $ do
      let j0 = sinOsc AR 440 0
          j1 = wrapOut (wrapOut (wrapOut j0))
          h1 = S.out 0 (S.sinOsc AR 440 0)
      same_graph j1 h1

--
-- Exported
--

hsc3Tests :: Spec
hsc3Tests =
  describe "hsc3"
           (do simple_graph
               mix_mce_graph
               nondet_graph
               mrg_graph
               enum_graph
               controls_graph
               unary_op_graph
               optimize_graph
               demand_graph
               envelope_graph
               pv_graph
               handwritten_graph)
