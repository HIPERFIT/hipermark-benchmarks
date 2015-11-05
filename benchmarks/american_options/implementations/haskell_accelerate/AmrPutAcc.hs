{-# LANGUAGE FlexibleContexts #-}
module AmrPutAcc
       (binom, main)

where
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate (Z(..), (:.)(..))

import qualified Data.Array.Accelerate.CUDA as ACUDA

import Control.DeepSeq (deepseq, NFData)
import Control.Monad
import System.Environment (getEnv)
import System.CPUTime

-- Pointwise manipulation of vectors an scalars
(^*^), (^+^) :: A.Acc (A.Vector FloatRep) -> A.Acc (A.Vector FloatRep) -> A.Acc (A.Vector FloatRep)
v1 ^*^ v2 = A.zipWith (*) v1 v2
v1 ^+^ v2 = A.zipWith (+) v1 v2
(-^), (*^) :: A.Exp FloatRep -> A.Acc (A.Vector FloatRep) -> A.Acc (A.Vector FloatRep)
c -^ v = A.map (c -) v
c *^ v = A.map (c *) v

pmax :: A.Acc (A.Vector FloatRep) -> A.Exp FloatRep -> A.Acc (A.Vector FloatRep)
pmax v c = A.map (max c) v

ppmax :: A.Acc (A.Vector FloatRep) -> A.Acc (A.Vector FloatRep) -> A.Acc (A.Vector FloatRep)
ppmax = A.zipWith max

type FloatRep = Float
--type FloatRep = Double
-- I would like to use Double, but my NVIDA card does not support double


binom :: A.Exp Int -> A.Acc(A.Vector FloatRep)
binom expiry = first
  where
    i2f :: A.Exp A.DIM1 -> A.Exp FloatRep
    i2f = A.fromIntegral . A.unindex1

    uPow, dPow :: A.Exp Int -> A.Acc(A.Vector FloatRep)
    dPow i = A.drop (n+1-i)
           $ A.reverse
           $ A.generate (A.lift (Z:.n+1)) (\ix -> d ** i2f ix)

    uPow i = A.take i
           $ A.generate (A.lift (Z:.n+1)) (\ix -> u ** i2f ix)

    finalPut :: A.Acc (A.Vector FloatRep)
    finalPut = pmax (A.fromIntegral strike -^ st) (0.0)
        where st = A.fromIntegral s0 *^ (uPow (n+1)^*^ dPow (n+1))

-- for (i in n:1) {
--   St<-S0*u.pow[1:i]*d.pow[i:1]
--   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }

    first :: A.Acc (A.Vector FloatRep)
    first = A.afst $ A.awhile
              (\ x -> A.unit $ A.the (A.asnd x) A.>* 0)
              (\ x -> A.lift (prevPut (A.the (A.asnd x)) (A.afst x) , A.map (+(-1)) (A.asnd x)))
              (A.lift (finalPut, A.unit n))

    prevPut :: A.Exp Int -> A.Acc(A.Vector FloatRep) -> A.Acc(A.Vector FloatRep)
    prevPut i put =
      ppmax(A.fromIntegral strike -^ st) ((qUR *^ A.tail put) ^+^ (qDR *^ A.init put))
        where st = A.fromIntegral s0 *^ (uPow i ^*^ dPow i)

    -- standard econ parameters

    strike, bankDays, s0 :: A.Exp Int
    strike = 100
    bankDays = 252
    s0 = 100

    r, alpha, sigma :: A.Exp FloatRep
    r = 0.03; alpha = 0.07; sigma = 0.20

    n :: A.Exp Int
    n = expiry*bankDays

    dt, u, d, stepR, q, qUR :: A.Exp FloatRep
    dt = A.fromIntegral expiry / A.fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR


arun :: (t -> A.Vector a) -> t -> a
arun run x = head $ A.toList $ run x

time :: NFData t => t -> IO (t, Integer)
time x = do
  start <- getCPUTime -- In picoseconds; 1 microsecond == 10^6 picoseconds.
  end <- x `deepseq` getCPUTime
  return (x, (end - start) `div` 1000000)

main :: IO ()
main = do
  n <- liftM read getContents
  (v, runtime) <- time $ arun ACUDA.run $ binom $ A.constant n
  result <- getEnv "HIPERMARK_RESULT"
  writeFile result $ show v
  runtime_file <- getEnv "HIPERMARK_RUNTIME"
  writeFile runtime_file $ show runtime
