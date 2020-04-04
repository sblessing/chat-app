use "random"

class SimpleRand is Random
  var _value: U64

  new create(x: U64, y: U64 = 0) =>
    _value = x

  fun ref next(): U64 =>
    nextLong()

  fun ref nextLong(): U64 =>
    _value = ((_value * 1309) + 13849) and 65535
    _value
  
  fun ref nextInt(max: U32 = 0): U32 =>
    if max == 0 then
      nextLong().u32()
    else
      nextLong().u32() % max
    end
  
  fun ref nextDouble(): F64 =>
    1.0 / (nextLong() + 1).f64()

class CongruentialRand is Random
  var _x: U64
  var _next_gaussian: F64 = 0
  var _has_next_gaussian: Bool = false

  new create(x: U64, y: U64 = 0) =>
    _x = (x xor U64(0x5DEECE66D)) and ((U64(1) << 48) -1)
    
  fun ref next_mask(bits: U64): U64 =>
    next() >> U64(48 - bits)

  fun ref next(): U64 =>
    """
    Congruential pseudorandom number generator,
    as defined by D.H. Lehmer and described by
    Donald E. Knuth.
    See The Art of Computer Programming, Vol. 3,
    Section 3.2.1
    """
    _x = ((_x * U64(0x5DEECE77D)) + U64(0xB)) and ((U64(1) << 48) - 1)

  fun ref nextBoolean(): Bool =>
    next_mask(1) != U64(0)

  fun ref nextDouble(): F64 =>
    (((next_mask(26) << 27).f64() + next_mask(27).f64()) / U64(1 << 53).f64())

  fun ref nextLong(): U64 =>
    (next_mask(32) << 32) + next_mask(32)

  fun ref nextGaussian(): F64 =>
    """
    Returns the next gaussian normally distributed
    random number with mean 0.0 and a standard 
    deviation of 1.0. Implemented using the polar
    method as described by G.E.P Box, M.E. Muller
    and G. Marsaglia.
    See The Art of Computer Programming, Vol. 3,
    Section 3.4.1
    """
    if _has_next_gaussian == true then
      _has_next_gaussian = false
      _next_gaussian
    else
      var v1: F64 = 0
      var v2: F64 = 0
      var s: F64 = 0

      repeat
        v1 = (2 * nextDouble()) - 1
        v2 = (2 * nextDouble()) - 1
        s = (v1 * v1) + (v2 * v2)
      until ((s < 1) and (s != 0)) end

      let multiplier = F64(-2 * (s.log()/s)).sqrt()
      _next_gaussian = v2 * multiplier
      _has_next_gaussian = true
      v1 * multiplier
    end

class DiceRoll
  let _random: SimpleRand

  new create(seed: U64) =>
    _random = SimpleRand(seed)

  fun ref apply(probability: U64): Bool =>
    _random.nextInt(100) < probability.u32()
