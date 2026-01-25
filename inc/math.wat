   ;; ANDY MATH RUNTIME  ──────────────────────────────────────────────────

   ;; 1/4 Sine Wave lookup table:
   ;; f64 values for sin(x), 0 ≤ x ≤ π/2, step = 0.01 rad
   ;; Table length = 158 entries (0–1.57 radians)
   ;; Each entry is 8 bytes (f64), total = 1264 bytes
   ;; Accuracy is within ~5×10⁻⁵ (≈ 0.003°)
   ;; Base address = 4096 (0x1000)
   (data (i32.const 4096)
     "\00\00\00\00\00\00\00\00" "\07\E8\15\E9\CA\7A\84\3F" "\67\58\A5\CD\87\7A\94\3F" "\C6\67\96\F1\23\B7\9E\3F"
     "\FB\72\0A\65\7B\79\A4\3F" "\3C\64\FF\A2\DE\96\A9\3F" "\96\5E\88\2D\9A\B3\AE\3F" "\01\61\E7\41\C6\E7\B1\3F"
     "\16\44\11\15\4A\75\B4\3F" "\B5\D9\6A\D5\47\02\B7\3F" "\2C\CB\8B\CB\AE\8E\B9\3F" "\2B\F7\E7\43\6E\1A\BC\3F"
     "\CD\E5\3C\8F\75\A5\BE\3F" "\3C\90\7F\01\DA\97\C0\3F" "\6F\B5\E3\7C\8C\DC\C1\3F" "\22\ED\DF\E9\C9\20\C3\3F"
     "\2F\64\88\FB\89\64\C4\3F" "\64\CF\26\68\C4\A7\C5\3F" "\45\BC\70\E9\70\EA\C6\3F" "\5D\CB\BD\3C\87\2C\C8\3F"
     "\BC\D2\3D\23\FF\6D\C9\3F" "\47\E7\2E\62\D0\AE\CA\3F" "\77\4B\13\C3\F2\EE\CB\3F" "\18\42\E7\13\5E\2E\CD\3F"
     "\BC\C3\56\27\0A\6D\CE\3F" "\77\15\F3\D4\EE\AA\CF\3F" "\BD\1F\B4\FC\01\74\D0\3F" "\24\31\59\BB\20\12\D1\3F"
     "\8A\74\29\9A\CF\AF\D1\3F" "\F6\CB\C2\8F\0A\4D\D2\3F" "\33\BA\BA\95\CD\E9\D2\3F" "\AF\C3\B8\A8\14\86\D3\3F"
     "\3A\BB\90\C8\DB\21\D4\3F" "\01\FA\5C\F8\1E\BD\D4\3F" "\08\82\98\3E\DA\57\D5\3F" "\8C\0A\39\A5\09\F2\D5\3F"
     "\89\F5\C8\39\A9\8B\D6\3F" "\DF\2D\81\0D\B5\24\D7\3F" "\39\ED\62\35\29\BD\D7\3F" "\45\69\51\CA\01\55\D8\3F"
     "\6B\67\2B\E9\3A\EC\D8\3F" "\6C\B6\E4\B2\D0\82\D9\3F" "\43\8D\9F\4C\BF\18\DA\3F" "\A5\CE\C5\DF\02\AE\DA\3F"
     "\61\30\22\9A\97\42\DB\3F" "\25\46\F9\AD\79\D6\DB\3F" "\E0\6E\22\52\A5\69\DC\3F" "\31\A4\20\C2\16\FC\DC\3F"
     "\35\2B\3B\3E\CA\8D\DD\3F" "\20\26\96\0B\BC\1E\DE\3F" "\F0\05\4B\74\E8\AE\DE\3F" "\A3\DB\80\C7\4B\3E\DF\3F"
     "\56\88\84\59\E2\CC\DF\3F" "\CD\65\F0\41\54\2D\E0\3F" "\C2\17\BB\52\CD\73\E0\3F" "\D4\68\49\91\DA\B9\E0\3F"
     "\52\04\85\32\7A\FF\E0\3F" "\B3\F2\25\6E\AA\44\E1\3F" "\D4\47\BE\7E\69\89\E1\3F" "\92\BE\C5\A1\B5\CD\E1\3F"
     "\59\41\A5\17\8D\11\E2\3F" "\74\5F\C2\23\EE\54\E2\3F" "\C7\AE\8A\0C\D7\97\E2\3F" "\AC\19\7F\1B\46\DA\E2\3F"
     "\A8\18\3F\9D\39\1C\E3\3F" "\AB\D7\93\E1\AF\5D\E3\3F" "\A0\46\7B\3B\A7\9E\E3\3F" "\EC\14\33\01\1E\DF\E3\3F"
     "\AD\97\43\8C\12\1F\E4\3F" "\61\9A\8A\39\83\5E\E4\3F" "\B9\19\46\69\6E\9D\E4\3F" "\43\E8\1E\7F\D2\DB\E4\3F"
     "\BE\3C\33\E2\AD\19\E5\3F" "\B1\29\21\FD\FE\56\E5\3F" "\20\FE\10\3E\C4\93\E5\3F" "\0D\8F\BF\16\FC\CF\E5\3F"
     "\7D\69\88\FC\A4\0B\E6\3F" "\C6\EC\6F\68\BD\46\E6\3F" "\E4\4C\2D\D7\43\81\E6\3F" "\8E\7C\34\C9\36\BB\E6\3F"
     "\CD\FE\BF\C2\94\F4\E6\3F" "\D7\9F\DA\4B\5C\2D\E7\3F" "\E9\14\69\F0\8B\65\E7\3F" "\E7\82\33\40\22\9D\E7\3F"
     "\7B\EB\EE\CE\1D\D4\E7\3F" "\83\80\46\34\7D\0A\E8\3F" "\7E\DD\E4\0B\3F\40\E8\3F" "\D0\26\7D\F5\61\75\E8\3F"
     "\90\0E\D4\94\E4\A9\E8\3F" "\B0\BE\C8\91\C5\DD\E8\3F" "\41\A8\5D\98\03\11\E9\3F" "\94\37\C1\58\9D\43\E9\3F"
     "\0A\6D\56\87\91\75\E9\3F" "\54\5A\BD\DC\DE\A6\E9\3F" "\E5\83\DB\15\84\D7\E9\3F" "\6F\26\E4\F3\7F\07\EA\3F"
     "\28\60\60\3C\D1\36\EA\3F" "\A3\3D\37\B9\76\65\EA\3F" "\0E\AA\B5\38\6F\93\EA\3F" "\9D\42\96\8D\B9\C0\EA\3F"
     "\EE\0C\09\8F\54\ED\EA\3F" "\36\10\BB\18\3F\19\EB\3F" "\03\D1\DD\0A\78\44\EB\3F" "\62\AF\2E\4A\FE\6E\EB\3F"
     "\3F\27\FE\BF\D0\98\EB\3F" "\C1\F2\36\5A\EE\C1\EB\3F" "\83\0E\65\0B\56\EA\EB\3F" "\77\9F\BC\CA\06\12\EC\3F"
     "\3B\BA\20\94\FF\38\EC\3F" "\C7\0B\2A\68\3F\5F\EC\3F" "\38\63\2D\4C\C5\84\EC\3F" "\93\1C\42\4A\90\A9\EC\3F"
     "\5E\6C\48\71\9F\CD\EC\3F" "\CE\8B\EF\D4\F1\F0\EC\3F" "\86\C5\BB\8D\86\13\ED\3F" "\99\62\0C\B9\5C\35\ED\3F"
     "\C3\77\21\79\73\56\ED\3F" "\AC\92\21\F5\C9\76\ED\3F" "\01\47\1F\59\5F\96\ED\3F" "\5E\9B\1E\D6\32\B5\ED\3F"
     "\C4\55\1A\A2\43\D3\ED\3F" "\8C\27\09\F8\90\F0\ED\3F" "\B3\B8\E2\17\1A\0D\EE\3F" "\54\92\A4\46\DE\28\EE\3F"
     "\42\E8\56\CE\DC\43\EE\3F" "\8C\41\11\FE\14\5E\EE\3F" "\DE\FF\FE\29\86\77\EE\3F" "\9C\C5\63\AB\2F\90\EE\3F"
     "\95\BA\9F\E0\10\A8\EE\3F" "\3F\AF\33\2D\29\BF\EE\3F" "\4B\1E\C5\F9\77\D5\EE\3F" "\93\0C\22\B4\FC\EA\EE\3F"
     "\30\C7\44\CF\B6\FF\EE\3F" "\AC\7F\57\C3\A5\13\EF\3F" "\35\C6\B7\0D\C9\26\EF\3F" "\BE\E1\F9\30\20\39\EF\3F"
     "\F1\05\EC\B4\AA\4A\EF\3F" "\E0\66\99\26\68\5B\EF\3F" "\6B\2A\4D\18\58\6B\EF\3F" "\2D\37\95\21\7A\7A\EF\3F"
     "\03\E1\44\DF\CD\88\EF\3F" "\F7\72\77\F3\52\96\EF\3F" "\9A\96\92\05\09\A3\EF\3F" "\AD\98\48\C2\EF\AE\EF\3F"
     "\0D\8B\9A\DB\06\BA\EF\3F" "\DC\43\DA\08\4E\C4\EF\3F" "\D1\39\AC\06\C5\CD\EF\3F" "\A5\3D\09\97\6B\D6\EF\3F"
     "\95\10\40\81\41\DE\EF\3F" "\ED\D7\F6\91\46\E5\EF\3F" "\8B\6D\2C\9B\7A\EB\EF\3F" "\62\8D\39\74\DD\F0\EF\3F"
     "\E4\DF\D1\F9\6E\F5\EF\3F" "\57\E1\04\0E\2F\F9\EF\3F" "\0A\A6\3E\98\1D\FC\EF\3F" "\68\7B\48\85\3A\FE\EF\3F"
     "\E2\65\49\C7\85\FF\EF\3F" "\B1\7B\C6\55\FF\FF\EF\3F")
   

   ;; X modulus 2pi
   ;; This is a helper function for the trig functions,
   ;; which should only operate over x between 0 and 2*pi
   ;; WASM does not have a modulus operator, so this fixes that.
   ;; x = x - floor(x / (2π)) * (2π)
   (func $f64.mod2pi (param $x f64) (result f64)
     (local $result f64)
     local.get $x
     local.get $x
     f64.const 6.283185307179586
     f64.div
     f64.floor
     f64.const 6.283185307179586
     f64.mul
     f64.sub
     local.tee $result
     f64.const 0
     f64.lt
     (if (result f64)
       (then
         local.get $result
         f64.const 6.283185307179586
         f64.add
        ) (else
         local.get $result
        )
     )
   )


   ;; SIN
   ;; Calculate sin using 1/4 sin-wave lookup table
   ;;   sin(x) = sin(x)               for 0 ≤ x ≤ π/2
   ;;   sin(x) = sin(π - x)           for π/2 ≤ x ≤ π
   ;;   sin(x) = -sin(x - π)          for π ≤ x ≤ 3π/2
   ;;   sin(x) = -sin(2π - x)         for 3π/2 ≤ x ≤ 2π
   ;;
   ;; Uses linear interpolation between table entries
   (func $sin (param $x f64) (result f64)
   
     (local $idx i32)           ;; Table Index
     (local $frac f64)          ;; % between v1 and v2
     (local $v1 f64)            ;; Lower Table Value
     (local $v2 f64)            ;; Upper Table Value
     (local $val f64)           ;; Interpolated value
     (local $neg i32)           ;; Neg portion of wave?
   
     ;; normalize x into [0, 2π)
     local.get $x
     call $f64.mod2pi     
     local.set $x
   
     ;; Determine quadrant and mirror if needed
     (if (f64.gt (local.get $x) (f64.const 4.71238898)) ;; > 3π/2
       (then
         f64.const 6.283185307179586
         local.get $x
         f64.sub
         local.set $x
         i32.const -1
         local.set $neg))
     (if (f64.gt (local.get $x) (f64.const 3.1415926535)) ;; > π
       (then
         local.get $x
         f64.const 3.1415926535
         f64.sub
         local.set $x
         i32.const -1
         local.set $neg))
     (if (f64.gt (local.get $x) (f64.const 1.57079632679)) ;; > π/2
       (then
         f64.const 3.1415926535
         local.get $x
         f64.sub
         local.set $x))
   
     ;; index = floor(x / 0.01)
     local.get $x
     f64.const 100
     f64.mul
     i32.trunc_f64_s
     local.set $idx
   
     ;; fractional offset
     local.get $x
     f64.const 0.01
     f64.div
     f64.floor
     f64.const 0.01
     f64.mul
     local.get $x
     f64.sub
     f64.const 100
     f64.mul
     local.set $frac
   
     ;; v1 = table[idx]
     local.get $idx
     i32.const 8
     i32.mul
     i32.const 4096
     i32.add
     f64.load
     local.set $v1
   
     ;; v2 = table[idx+1]
     local.get $idx
     i32.const 1
     i32.add
     i32.const 8
     i32.mul
     i32.const 4096
     i32.add
     f64.load
     local.set $v2
   
     ;; val = v1 + (v2 - v1) * frac  (linear interpolation)
     local.get $v2
     local.get $v1
     f64.sub
     local.get $frac
     f64.mul
     local.get $v1
     f64.add
     local.set $val
   
     ;; Apply sign if needed
     (if (i32.eq (local.get $neg) (i32.const -1))
       (then
         local.get $val
         f64.neg
         return))
   
     local.get $val)

   ;; COS
   ;; The cosine is calculated from a
   ;; π/2 phase shifted sin
   (func $cos (param $x f64) (result f64)
     local.get $x
     f64.const 1.57079632679  ;; π/2
     f64.add
     call $sin)
   
   ;; TAN
   ;; The tangent is the ratio of sin/cos
   (func $tan (param $x f64) (result f64)
     local.get $x
     call $sin
     local.get $x
     call $cos
     f64.div)

   ;; EXP
   ;; Implements e^x
   ;; Solved by a 5 term taylor series expansion
   ;; gives ~1e-5 accuracy for |x| ≤ 5
   ;; Can be unrolled for speedup
   ;; Additional iterations can be added for increased accuracy
   (func $exp (param $x f64) (result f64)     

     ;; Compute exp(x) ≈ 1 + x + x²/2! + x³/3! + x⁴/4! + x⁵/5!
     (local $term f64)        ;; (x^n)/n!
     (local $sum f64)         ;; running sum
     (local $n f64)           ;; the current n
     
     f64.const 1.0
     local.set $sum		;; sum = 1.0
     f64.const 1.0
     local.set $term            ;; term = 1.0
     f64.const 1.0
     local.set $n		;; n = 1.0

     ;; Compute successive taylor terms
     (loop $loop

       ;; term += x / n
       local.get $term
       local.get $x
       f64.mul
       local.get $n
       f64.div
       local.set $term

       ;; sum += term
       local.get $sum
       local.get $term
       f64.add
       local.set $sum

       ;; n++
       local.get $n
       f64.const 1.0
       f64.add
       local.set $n
 
       ;; stop after i > 5
       local.get $n
       f64.const 20.0
       f64.le
       br_if $loop
     )
 
     local.get $sum
   )

   ;; LN
   ;; Newtwon-Raphson Method
   ;; Error is ~1e-3 for x <= 10
   ;; Additional unrolled loops can be added for increased accuracy
   (func $ln (param $x f64) (result f64)
     ;; ln(x) via Newton iteration:
     ;; y₀ = x - 1
     ;; repeat twice: y = y + 2*(x - exp(y)) / (x + exp(y))
     (local $y f64)		;; the current y
     (local $ey f64)		;; e^y
     (local $num f64)		;; numerator: 2*(x - exp(y)) 
     (local $den f64)		;; denominator:  x + exp(y)
 
     local.get $x
     f64.const 1.0
     f64.sub
     local.set $y ;; initial guess

     ;; Unrolled i=5 loop for speed...

     ;; iteration 1
     local.get $y
     call $exp
     local.set $ey
     local.get $x
     local.get $ey
     f64.sub
     f64.const 2.0
     f64.mul
     local.set $num
     local.get $x
     local.get $ey
     f64.add
     local.set $den
     local.get $num
     local.get $den
     f64.div
     local.get $y
     f64.add
     local.set $y
 
     ;; iteration 2
     local.get $y
     call $exp
     local.set $ey
     local.get $x
     local.get $ey
     f64.sub
     f64.const 2.0
     f64.mul
     local.set $num
     local.get $x
     local.get $ey
     f64.add
     local.set $den
     local.get $num
     local.get $den
     f64.div
     local.get $y
     f64.add
     local.set $y

     ;; iteration 3
     local.get $y
     call $exp
     local.set $ey
     local.get $x
     local.get $ey
     f64.sub
     f64.const 2.0
     f64.mul
     local.set $num
     local.get $x
     local.get $ey
     f64.add
     local.set $den
     local.get $num
     local.get $den
     f64.div
     local.get $y
     f64.add
     local.set $y

     ;; iteration 4
     local.get $y
     call $exp
     local.set $ey
     local.get $x
     local.get $ey
     f64.sub
     f64.const 2.0
     f64.mul
     local.set $num
     local.get $x
     local.get $ey
     f64.add
     local.set $den
     local.get $num
     local.get $den
     f64.div
     local.get $y
     f64.add
     local.set $y

     ;; iteration 5
     local.get $y
     call $exp
     local.set $ey
     local.get $x
     local.get $ey
     f64.sub
     f64.const 2.0
     f64.mul
     local.set $num
     local.get $x
     local.get $ey
     f64.add
     local.set $den
     local.get $num
     local.get $den
     f64.div
     local.get $y
     f64.add
     local.set $y

     local.get $y
   )

   ;; POW
   ;; pow(x,y) = x^y
   ;; where:
   ;;    x^y = e^(y*ln(x))
   ;; for:
   ;;    x > 0.0
   (func $pow (param $x f64) (param $y f64) (result f64)
     (local $tmp f64)
   
     local.get $x
     f64.const 0.0
     f64.le
     ;; if x <= 0.0, return 0.0
     (if (result f64)
         (then (f64.const 0.0))
     ;; otherwise, return exp(y*ln(x))
         (else
           ;; tmp = ln(x)
           local.get $x
           call $ln
           local.set $tmp
   
           ;; tmp = y * ln(x)
           local.get $y
           local.get $tmp
           f64.mul
           local.set $tmp
   
           ;; return exp(y * ln(x))
           local.get $tmp
           call $exp
         )
     )
   )

   ;; END MATH RUNTIME  ───────────────────────────────────────────────────

