> {-# LANGUAGE Arrows #-}
> module JaneStreetLesson where
> import Euterpea
> import Control.Arrow ((>>>),(<<<),arr)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Name: Haohang Xu
Class Date: 8/10/2015

Description: Example code for Jane Street Summer 2015 5-minute classes.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**************************
**    SIMPLE WAVES      **
**************************

--- A square wave! --- 

> squareWaveTable :: Table
> squareWaveTable = tableLinearN 4096 1 [(1, 1), (0, -1), (1, -1)] 

> squareWave :: SigFun AudRate () Double
> squareWave = proc () -> do
>   oscI squareWaveTable 0 -< 440

> squareWaveOut = outFile "square_wave.wav" 5 squareWave

--- A sine wave! ---

> sineWaveTable :: Table
> sineWaveTable = tableSinesN 4096 [1]

> sineWave :: SigFun AudRate () Double
> sineWave = proc () -> do 
>   oscI sineWaveTable 0 -< 440 

> sineWaveOut = outFile "sine_wave.wav" 5 sineWave

*****************************
**    ADDITIVE SYNTHESIS   **
*****************************

--- Odd Overtones --- 

> genOddOvertonesVals :: Int -> [(Double, Double, Double)]
> genOddOvertonesVals 0 = [(1.0, 1.0, 0.0)]
> genOddOvertonesVals n = 
>   (m, 1.0/m, 0.0) : genOddOvertonesVals (n - 1)
>   where m = (fromIntegral n) * 2 + 1.0

> genOddOvertonesTable :: Int -> Table
> genOddOvertonesTable n = tableSines3N 4096 $ genOddOvertonesVals $ n

> genOddOvertonesSignal :: Int -> SigFun AudRate () Double
> genOddOvertonesSignal n = proc () -> do 
>   oscI (genOddOvertonesTable n) 0 -< 440 

> oddOvertonesOut n = outFile "odd_overtones.wav" 5 (genOddOvertonesSignal n)

--- Emulating the Sound of a Clarinet ---

> clarinetTable :: Table
> clarinetTable = tableSines3N 4096 [
>   (1,  1,    0), 
>   (3,  0.75, 0), 
>   (5,  0.5,  0), 
>   (7,  0.14, 0), 
>   (9,  0.5,  0), 
>   (11, 0.12, 0), 
>   (13, 0.17, 0)]

> clarinetSignal :: SigFun AudRate () Double
> clarinetSignal = proc () -> do
>   oscI clarinetTable 0 -< 440

> clarinetOut = outFile "clarinet.wav" 5 clarinetSignal

--- Emulating the Sound of a Violin ---

> violinTable :: Table
> violinTable = tableSines3N 4096 [
>   (1,  1,        0), 
>   (2,  1.2,      0), 
>   (3,  0.5,      0), 
>   (4,  0.05,     0), 
>   (5,  0.35,     0), 
>   (6,  0.35,     0), 
>   (7,  0.04,     0), 
>   (8,  0.6,      0), 
>   (9,  0.01,     0), 
>   (10, 0.025,    0), 
>   (11, 0.05,     0), 
>   (12, 0.0009,   0),
>   (13, 0.0005,   0), 
>   (14, 0.007,    0), 
>   (15, 0.001,    0), 
>   (16, 0.005,    0), 
>   (18, 0.0005,   0), 
>   (19, 0.00005,  0), 
>   (20, 0.0001,   0), 
>   (21, 0.0001,   0),
>   (22, 0.00005,  0), 
>   (23, 0.0002,   0), 
>   (24, 0.00003,  0), 
>   (25, 0.00006,  0), 
>   (28, 0.00005,  0), 
>   (29, 0.000047, 0), 
>   (30, 0.00003,  0), 
>   (33, 0.00003,  0)]

> violinSignal :: SigFun AudRate () Double
> violinSignal = proc () -> do
>   oscI violinTable 0 -< 440

> violinOut = outFile "violin.wav" 5 violinSignal

--- Emulating the Sound of a Bell (Simple) ---

> simpleBell  :: Instr (Mono AudRate)
>           -- |Dur -> AbsPitch -> Volume -> AudSF () Double|
> simpleBell dur ap vol [] = 
>  let  f    = apToHz ap
>       v    = fromIntegral vol / 100
>       d    = fromRational dur
>       sfs  = map  (\p-> constA (f*p) >>> osc oneSineWaveTable 0) 
>                   [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
>  in proc () -> do
>       aenv  <- envExponSeg [0,1,0.001] [0.003,d-0.003] -< ()
>       a1    <- foldSF (+) 0 sfs -< ()
>       outA  -< a1 * aenv * v/9

> oneSineWaveTable = tableSinesN 4096 [1]

> simpleBellOut = outFile "simple_bell.wav" 6 
>                 (simpleBell 6 (absPitch (A,4)) 100 []) 

*********************************************
**    FREQUENCY AND AMPLITUDE MODULATION   **
*********************************************

--- Emulating the Sound of a Bell (Sophisticated)---

> sophBell  :: Instr (Mono AudRate)
>              -- |Dur -> AbsPitch -> Volume -> AudSF () Double|
> sophBell dur ap vol [] = 
>  let  f    = apToHz ap
>       v    = fromIntegral vol / 100
>       d    = fromRational dur
>       sfs  = map  (mySF f d)
>                   [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
>  in proc () -> do
>       a1    <- foldSF (+) 0 sfs -< ()
>       outA  -< a1*v/9

> mySF f d p = proc () -> do
>              s     <- osc oneSineWaveTable 0 <<< constA (f*p) -< ()
>              aenv  <- envExponSeg [0, 1, 0.001] [0.003, d/p-0.003] -< ()
>              outA  -< s*aenv

> sophBellOut = outFile "sophisticated_bell.wav" 6 
>               (sophBell 6 (absPitch (A,4)) 100 []) 

--- Creating an "Electro" Sound with Frequency Modulation ---

> fourSineWavesTable :: Table
> fourSineWavesTable = tableSinesN 4098 [1, 1, 1, 1]

> electro :: Instr (Mono AudRate)
> electro dur ap vol [] = 
>   let f = apToHz ap 
>       v = fromIntegral vol/100
>   in proc () -> do 
>      var  <- osc oneSineWaveTable 0 -< 5
>      aud  <- osc fourSineWavesTable 0 -< f + 2 * var 
>      outA -< aud * v/5

> electroOut = outFile "electro.wav" 5 (electro 5 (absPitch (C, 5)) 100 [])

> nonElectro :: Instr (Mono AudRate) 
> nonElectro dur ap vol [] = 
>   let f = apToHz ap
>       v = fromIntegral vol/100
>   in proc () -> do
>       aud <- osc fourSineWavesTable 0 -< f
>       outA -< aud * v/5

> nonElectroOut = outFile "nonelectro.wav" 5 
>                 (nonElectro 5 (absPitch (C, 5)) 100 [])

***********************************************
**    COMBINING EVERYTHING WITH WAVEGUIDES   **
***********************************************

--- Emulating a Bongo Sound with Waveguides ---

> waveguide :: Double -> Double -> Double -> AudSF (Double,Double) (Double,Double)
> waveguide del ga gb = proc (ain, bin) -> do
>   rec bout <- delayLine del -< bin - ga * aout
>       aout <- delayLine del -< ain - gb * bout
>   outA -< (aout,bout)

> bongo :: (Double -> AudSF () Double) -> Instr (Mono AudRate)
> bongo stick _ ap v _ =
>   let freq = apToHz (fromIntegral ap)
>       vol = fromIntegral v/100
>   in proc () -> do
>       stk <- stick (1/freq) -< ()
>       rec (right1,left1) <- waveguide (1/freq) 0.6 0.6 -< ((0.5*stk+right3+up4)*0.5,left2)
>           (right2,left2) <- waveguide (1/freq) 0.6 0.6 -< (right1,left3)
>           (right3,left3) <- waveguide (1/freq) 0.6 0.6 -< (right2,(0.5*stk+left1+up4)*0.5)
>           (down4,up4) <- waveguide (1/freq) 0.6 0.6 -< ((left1+right3+stk)*0.5,down4)
>       outA -< (right1+left1+right2+left2+right3+left3+down4+up4)*vol

> triangleStick :: Double -> AudSF () Double
> triangleStick d =
>   proc () -> do
>       env <- envLineSeg [0,0.9,-0.9,0,0] [d/4, d/2, d/4,d] -< ()
>       outA -< env

> bongoTriangle :: Instr (Mono AudRate) 
> bongoTriangle = bongo triangleStick

> drumbeat :: Music Pitch
> drumbeat = line [c 4 qn, c 4 qn, c 4 qn, c 4 en, c 4 en, c 4 qn]

> bongoTriangleOut = uncurry (outFileNorm "bongo_triangle.wav") $ 
>                renderSF (instrument (Custom "bongotr") drumbeat)
>                [(Custom "bongotr", bongoTriangle)]

We can try some different sticks!

> squareStick :: Double -> AudSF () Double
> squareStick d =
>   proc () -> do
>       env <- envLineSeg [0,0.9,0.9,-0.9,-0.9,0,0] [0, d/2, 0, d/2, 0, d] -< ()
>       outA -< env

> bongoSquare :: Instr (Mono AudRate)
> bongoSquare = bongo squareStick

> bongoSquareOut = uncurry (outFileNorm "bongo_square.wav") $
>                  renderSF (instrument (Custom "bongosq") drumbeat)
>                  [(Custom "bongosq", bongoSquare)]

> sineStick :: Double -> AudSF () Double
> sineStick d = 
>   proc () -> do
>       s <- osc oneSineWaveTable 0 -< 1/d
>       dels <- delayLine d -< s
>       outA -< s - dels

> bongoSine :: Instr (Mono AudRate)
> bongoSine = bongo sineStick

> bongoSineOut = uncurry (outFileNorm "bongo_sine.wav") $
>                renderSF (instrument (Custom "bongosine") drumbeat)
>                [(Custom "bongosine", bongoSine)]

