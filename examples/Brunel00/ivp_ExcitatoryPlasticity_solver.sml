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


fun ivp_ExcitatoryPlasticity(input as {h,t,weight}) = 
let
val weightprime299 = (fn () => 0.0)
val dfn302 = (fn (t,yvec) => let 
val weight = Unsafe.Vector.sub (yvec, 0)

in 
(Vector.fromList [weightprime299()])
end
)
val integral300 = (integral(dfn302,(#t(input)),(Vector.fromList [(#weight(input))]),(#h(input)),0))
val ysn303 = (#ysn(integral300))
val xn304 = (#xn(integral300))
val integral301 = {t = xn304, weight_t_h296_297 = Unsafe.Vector.sub (ysn303, 0)}
val actuate305 = {t = (#t(integral301)), weight = (#weight_t_h296_297(integral301))}

in
actuate305
end
end
