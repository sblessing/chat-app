use "collections"

class SampleStats
  var _samples: Array[F64]

  new create(samples: Array[F64]) =>
    _samples = samples

  fun ref sum(): F64 =>
    var s: F64 = 0

    try
      for i in Range(0, _samples.size()) do
        s = s + _samples(i)?
      end
    end

    s

  fun ref mean(): F64 =>
    (sum() / _samples.size().f64())

  fun ref median(): F64 =>
    let size = _samples.size() 

    if size == 0 then
      0
    else
      let middle = size / 2

      try
        if (size % 2) == 1 then
          _samples(middle)?
        else
          (_samples(middle - 1)? + _samples(middle)?) / 2
        end
      else
        0
      end
    end

  fun ref geometric_mean(): F64 =>
    var result: F64 = 0

    for i in Range[USize](0, _samples.size()) do
       try result = result + _samples(i)?.log10() end
    end
    
    F64(10).pow(result / _samples.size().f64())

  fun ref harmonic_mean(): F64 =>
    var denom: F64 = 0

    for i in Range[USize](0, _samples.size()) do
      try denom = denom + ( 1 / _samples(i)?) end
    end

    _samples.size().f64() / denom

  fun ref stddev(): F64 =>
    let m = mean()
    var temp: F64 = 0

    for i in Range[USize](0, _samples.size()) do
      try 
        let sample = _samples(i)?
        temp = temp + ((m - sample) * (m - sample))
      end
    end

    (temp / _samples.size().f64()).sqrt()
  
  fun ref err(): F64 =>
    F64(100) * ((confidence_high() - mean()) / mean())

  fun ref variation(): F64 =>
   stddev() / mean()

  fun ref confidence_low(): F64 =>
    mean() - (F64(1.96) * (stddev() / _samples.size().f64().sqrt()))

  fun ref confidence_high(): F64 =>
    mean() + (F64(1.96) * (stddev() / _samples.size().f64().sqrt()))

   fun ref skewness(): F64 =>
     let m = mean()
     let sd = stddev()
     var total: F64 = 0
     var diff: F64 = 0

     if _samples.size() > 0 then
       for i in Range[USize](0, _samples.size()) do
         try
           diff = _samples(i)? - m
           total = total + (diff * diff * diff)
         end
       end

       total / ((_samples.size().f64() - 1) * sd * sd * sd) 
     else
       0
     end