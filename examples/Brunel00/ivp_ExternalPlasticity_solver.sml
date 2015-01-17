structure Model = 
struct

open Real
open Math
open RungeKutta

datatype ('b,'c) trs = TRSA of 'b | TRSB of 'c
datatype ('a,'b,'c) trc = TRC of ((('a -> (('b,'c) trs))) * 
                                  (('a -> (('b,'c) trs))) * 
                                  ((('b,'c) trs) -> real))
	 
fun tsCase (fa,fb,x) = case x of TRSA a => (fa a) | TRSB b => (fb b)
fun trfOf x = case x of TRC (f,fk,e) => f
fun trfkOf x = case x of TRC (f,fk,e) => fk
fun treOf x = case x of TRC (f,fk,e) => e

fun putStrLn str = 
  (TextIO.output (TextIO.stdOut, str);
   TextIO.output (TextIO.stdOut, "\n"))

fun putStr str = (TextIO.output (TextIO.stdOut, str))

fun showReal n = 
let open StringCvt
in
(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
end

fun vmap2 f (v1,v2) = 
    let 
        val n = Vector.length v1
    in
        Vector.tabulate (n, fn (i) => f (Unsafe.Vector.sub (v1,i),
                                        Unsafe.Vector.sub (v2,i)))
    end

exception EmptySignal

val neg = (op ~)
val swap = fn (x,v) => (case v of NONE => x | SOME v => v) 
val equal = fn (x,y) => (x = y) 
val signalOf = fn (v) => (case v of NONE => raise EmptySignal | SOME v => v) 
val heaviside = fn (v) => (if Real.< (v, 0.0) then 0.0 else 1.0)

fun RandomInit () = RandomMTZig.fromEntropy()

val RandomState = RandomInit ()
val RandomZT = RandomMTZig.ztnew()

fun random_uniform () = RandomMTZig.randUniform RandomState
fun random_exponential (mu) = mu * RandomMTZig.randExp (RandomState, RandomZT)

val summer = fn (a,b) => (vmap2 (fn (x,y) => x+y) (a,b))
val scaler = fn(a,lst) => (Vector.map (fn (x) => a*x) lst)
val rkfe: (real vector) stepper1 = make_rkfe()
fun make_stepper (deriv) = rkfe (scaler,summer,deriv)
fun integral (f,x: real,y: real vector,h,i) = ((make_stepper f) h) (x,y)

fun eintegral (f,x,ys,evtest,h,i) =
    let val ysn = ((make_stepper f) h) (x,ys) 
    in
        ({xn=x+h,h=h,ysn=ysn})
    end


fun ivp_ExternalPlasticity(input as {h,t,weight}) = 
let
val weightprime289 = (fn () => 0.0)
val dfn292 = (fn (t,yvec) => let 
val weight = Unsafe.Vector.sub (yvec, 0)

in 
(Vector.fromList [weightprime289()])
end
)
val integral290 = (integral(dfn292,(#t(input)),(Vector.fromList [(#weight(input))]),(#h(input)),0))
val ysn293 = (#ysn(integral290))
val xn294 = (#xn(integral290))
val integral291 = {t = xn294, weight_t_h286_287 = Unsafe.Vector.sub (ysn293, 0)}
val actuate295 = {t = (#t(integral291)), weight = (#weight_t_h286_287(integral291))}

in
actuate295
end
end
