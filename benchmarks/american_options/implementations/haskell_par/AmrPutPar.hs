{-# LANGUAGE BangPatterns #-}
module AmrPutPar
       (binom, main)
where
import qualified Data.Vector.Unboxed as V

--import qualified Control.Monad.Par as P
import qualified Control.Monad.Par.Scheds.Direct as P
import Control.DeepSeq
import Control.Monad
import Data.List(foldl')
import System.Environment (getEnv)
import System.CPUTime

type FloatRep = Float

--instance V.Unbox a => NFData (V.Vector a)


-- Pointwise manipulation of vectors and scalars
(^*^), (^+^) :: (Num c, V.Unbox c) =>
                V.Vector c -> V.Vector c -> V.Vector c
v1 ^*^ v2 = V.zipWith (*) v1 v2
v1 ^+^ v2 = V.zipWith (+) v1 v2
(-^), (*^) :: (Num b, V.Unbox b) =>
              b -> V.Vector b -> V.Vector b
c -^ v = V.map (c -) v
c *^ v = V.map (c *) v

pmax :: (Ord b, V.Unbox b) =>
        V.Vector b -> b -> V.Vector b
pmax v c = V.map (max c) v
ppmax :: V.Vector FloatRep -> V.Vector FloatRep -> V.Vector FloatRep
ppmax = V.zipWith max 



binom :: Int -> FloatRep
binom expiry = V.head first
  where 
    !uPow = V.generate (n+1) (u^)
    !dPow = V.reverse $ V.generate (n+1) (d^)
    
    !st = s0 *^ (uPow ^*^ dPow)
    !finalPut = pmax (strike -^ st) 0

    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut put i = P.runPar $ do
      stx <- P.spawn.return $ strike -^ (s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow)))
      px  <- P.spawn.return $ ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      st'  <- P.get stx
      p   <- P.get px
      
      return $ ppmax st' p

    -- standard econ parameters
    strike = 100
    bankDays = 252
    s0 = 100 
    r = 0.03; alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR


time :: NFData t => t -> IO (t, Integer)
time x = do
  start <- getCPUTime -- In picoseconds; 1 microsecond == 10^6 picoseconds.
  end <- x `deepseq` getCPUTime
  return (x, (end - start) `div` 1000000)

main :: IO ()
main = do
  n <- liftM read getContents
  (v, runtime) <- time $ binom n
  result <- getEnv "HIPERMARK_RESULT"
  writeFile result $ show v
  runtime_file <- getEnv "HIPERMARK_RUNTIME"
  writeFile runtime_file $ show runtime
