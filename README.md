# hsc3-jiffy

## Introduction

This package contains efficient and scalable SynthDef builder functions
for `hsc3`.

The `hsc3` package has flexible and easy to use functions for defining
SynthDef graph for [SuperCollider][supercollider] synthesis engine.
However, the compilation time of SynthDef graphs with `hsc3` sometimes
show exponential growth. The `hsc3-jiffy` package contains `UGen`
binding functions and SynthDef builder with resembling interface to
`hsc3`, but internally uses different compilation approach to achieve
efficient and scalable SynthDef graph compilation.


## Motivating example

```Haskell
module Example01 where

import qualified Sound.SC3 as S
import qualified Sound.SC3.Jiffy

g1 :: Int -> S.UGen
g1 n =
  let d = S.dust 'a' KR 2
      dur = S.tExpRand 'b' 0.004 0.6 d
      o0 = S.lfTri AR (S.mce2 440 441) 0 * S.decay d dur
      f a b =
         let cg = S.coinGate a 0.033 d
             nr = S.tExpRand a 0.08 0.8 cg
             dt = S.exprange 5e-5 0.4 (S.lfdNoise3 a KR nr)
         in  clip2 (b + S.hpf (S.delayN b 0.4 dt * 0.5) 20) 1
      o1 = foldr f o0 [0 .. n]
  in  S.out 0 (o1 * 0.3)

g2 :: Int -> UGen
g2 n = do
  d <- share (dust KR 2)
  dur <- share (tExpRand 0.004 0.6 d)
  let o0 = lfTri AR (mce2 440 441) 0 * decay d dur
      f _ b = do
         let cg = coinGate 0.033 d
             nr = tExpRand 0.08 0.8 cg
         dt <- share (exprange 5e-5 0.4 (lfdNoise3 KR nr))
         b' <- share b
         clip2 (b' + hpf (delayN b' 0.4 dt * 0.8) 20) 1
      o1 = foldr f o0 [1..n]
  out 0 (o1 * 0.3)
```

Start `scsynth` with default port (i.e., UDP 57110), and load above in
`ghci`:

    ghci> :load Example01.hs
    ghci> :set +s

Sending the SynthDef graph defined with `hsc3`:

    ghci> audition (g1 11)
    (0.76 secs, 119,028,464 bytes)
    ghci> withSC3 stop
    (0.01 secs, 441,808 bytes)
    ghci> audition (g1 12)
    (1.58 secs, 228,329,688 bytes)
    ghci> withSC3 stop
    (0.01 secs, 441,808 bytes)

Sending `g1 11` took 0.76 seconds, and `g1 12` took 1.58 seconds. Note
the exponential growth with input size `n`.

Now sending the SynthDef graph defined with `hsc3-jiffy`:

    ghci> audition (g2 11)
    (0.03 secs, 6,715,456 bytes)
    ghci> withSC3 stop
    (0.01 secs, 441,808 bytes)
    ghci> audition (g2 12)
    (0.03 secs, 7,209,464 bytes)
    ghci> withSC3 stop
    (0.00 secs, 441,808 bytes)

Sending `g1 11` and `g1 12` took 0.03 seconds.

## Syntax and semantics

### Monadic UGen

Internally, `UGen` in `hsc3-jiffy` is defined as a monad containing
SynthDef graph state. This makes `UGen` an instance of `Functor`,
`Applicative`, and `Monad` type classes, and have different semantics
from `UGen` in `hsc3` package.

### UGen IDs

In `hsc3`, nondeterministic UGens (e.g., `dust`, `whiteNoise`), demand
UGens, and `localBuf` UGen use extra `ID` parameter to differentiate
UGen instances in a SynthDef graph. In `hsc3-jiffy`, those UGens are
differentiated by default. When sharing is desired, need to apply the
`share` function.

Following `hsc3` code:

```Haskell
nd1 :: S.UGen
nd1 =
  let w x = S.whiteNoise x AR
  in  S.out 0 (S.mce2 (w 'a' - w 'a') ((w 'b' - w 'c') * 0.1))
```

could be written as below with using functions from `hsc3-jiffy`:

```Haskell
nd2 :: UGen
nd2 = do
  let w = whiteNoise AR
  w0 <- share w
  out 0 (mce2 (w0 - w0) ((w - w) * 0.1))
```

### Multiple root graphs

Since `UGen` is an instance of `Monad`, code to create multiple root
graph in `hsc3` and `hsc3-jiffy` also differs.

Following `hsc3` graph:

```Haskell
mr1 :: S.UGen
mr1 =
  let d = S.dust 'a' KR 0.5
      a = S.freeSelf d
      b = S.out 0 (S.sinOsc AR 440 0 * 0.1)
  in  S.mrg [a,b]
```

could be written as below:

```Haskell
mr2 :: UGen
mr2 = do
  let d = dust KR 0.5
  _ <- freeSelf d
  out 0 (sinOsc AR 440 0 * 0.1)
```

## Order of UGens with multichannel expansion

Multichannel expansions in `hsc3` are done in depth-first order.  In
`hsc3-jiffy`, multichannel expansions are done in breadth-first order,
to maximize the compilation speed.  According to the [SuperCollider
SynthDef specification][synthdef_spec], the result may not the most
optimal graph when running in `scsynth` server.

The difference could be observed via `dumpUGens` function:

    ghci> :i dumpUGens
    class DumpUGens a where
      ...
      dumpUGens :: a -> IO ()

Following two graphs result in the same sound, but the order of `UGen`s
in the compiled result differs:

```Haskell
mc1 :: S.UGen
mc1 =
  let o = S.sinOsc AR (S.mce2 440 441) 0
  in  S.out 0 (o * 0.1)

mc2 :: UGen
mc2 =
  let o = sinOsc AR (mce2 440 441) 0
  in  out 0 (o * 0.1)
```

Viewing the graph written with `hsc3` functions:

    ghci> dumpUGens mc1
    <dump>
    [ 0_SinOsc, audio, [ 440.0, 0.0 ] ]
    [ 1_*, audio, [ 0_SinOsc, 0.1 ] ]
    [ 2_SinOsc, audio, [ 441.0, 0.0 ] ]
    [ 3_*, audio, [ 2_SinOsc, 0.1 ] ]
    [ 4_Out, audio, [ 0.0, 1_*, 3_* ] ]
    (0.01 secs, 552,816 bytes)

And the graph written with `hsc3-jiffy` functions:

    ghci> dumpUGens mc2
    <dump>
    [ 0_SinOsc, audio, [ 440.0, 0.0 ] ]
    [ 1_SinOsc, audio, [ 441.0, 0.0 ] ]
    [ 2_*, audio, [ 0_SinOsc, 0.1 ] ]
    [ 3_*, audio, [ 1_SinOsc, 0.1 ] ]
    [ 4_Out, audio, [ 0.0, 2_*, 3_* ] ]
    (0.01 secs, 679,264 bytes)

In the graph made with `hsc3-jiffy`, the two `SinOsc` UGens, which were
made from multichannel frequency inputs, appear earlier than
multiplication operators.


[supercollider]: https://supercollider.github.io
[synthdef_spec]: http://doc.sccode.org/Reference/Synth-Definition-File-Format.html
