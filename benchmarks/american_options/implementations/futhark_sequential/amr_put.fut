-- Port of Ken Friis Larsens pricer for American Put Options:
--
-- https://github.com/kfl/american-options.
--
-- This implementation is a straightforward sequential port - it is
-- not parallel, but it is in-place.

-- constants

fun int strike() = 100
fun int bankDays() = 252
fun int s0() = 100
fun real r() = 0.03
fun real alpha() = 0.07
fun real sigma() = 0.20

fun real maxReal(real x, real y) =
  if x < y then y else x

fun real binom(int expiry) =
  let n = expiry * bankDays() in
  let dt = toFloat(expiry) / toFloat(n) in
  let u = exp(alpha()*dt+sigma()*sqrt(dt)) in
  let d = exp(alpha()*dt-sigma()*sqrt(dt)) in
  let stepR = exp(r()*dt) in
  let q = (stepR-d)/(u-d) in
  let qUR = q/stepR in
  let qDR = (1.0-q)/stepR in

  let uPow = map(u **, map(toFloat, iota(n+1))) in
  let dPow = map(d **, map(toFloat, map(n-, iota(n+1)))) in
  let st = map(toFloat(s0())*, map(*, zip(uPow, dPow))) in
  let put = copy(map(maxReal(0.0), map(toFloat(strike())-, st))) in
  loop (put) = for n+1 > i >= 1 do
    loop (put) = for j < i do
      let st_j = toFloat(s0()) * (uPow[j] * dPow[n+1-i+j]) in
      let x = toFloat(strike()) - st_j in
      let y = qUR * put[j+1] + qDR * put[j] in
      let put[j] = maxReal(x,y) in
      put in
    put in
  put[0]

fun real main(int expiry) =
  binom(expiry)
