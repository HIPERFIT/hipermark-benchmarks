module AmrPut
       (binom, main)
where
import qualified Data.Vector.Unboxed as V
import Control.DeepSeq (deepseq, NFData)
import Control.Monad

import Data.List(foldl')
import System.Environment (getArgs, getEnv)
import System.CPUTime

import qualified Criterion.Main as C


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

ppmax :: V.Vector Double -> V.Vector Double -> V.Vector Double
ppmax = V.zipWith max


binom :: Int -> Double
binom expiry = V.head first
  where
    uPow = V.generate (n+1) (u^)
    dPow = V.reverse $ V.generate (n+1) (d^)

    st = s0 *^ (uPow ^*^ dPow)
    finalPut = pmax (strike -^ st) 0

-- for (i in n:1) {
--   St<-S0*u.pow[1:i]*d.pow[i:1]
--   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }
    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut put i = ppmax(strike -^ st') ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      where st' = s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow))

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
  args <- getArgs
  case args of
    [] -> do n <- liftM read getContents
             (v, runtime) <- time $ binom n
             result <- getEnv "HIPERMARK_RESULT"
             writeFile result $ show v
             runtime_file <- getEnv "HIPERMARK_RUNTIME"
             writeFile runtime_file $ show runtime
    ["-f"] -> print $ binom 128
    ["-t"] -> print $ map binom [1, 8, 16, 30, 64, 128]
    _ -> do
      let benchmarks = [ C.bench (show years) $ C.nf binom years
                       | years <- [1, 16, 30,  64, 128]]
      C.defaultMain benchmarks
