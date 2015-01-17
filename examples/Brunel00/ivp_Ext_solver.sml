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

val transientg262 = (fn (transient258,rate,t) => let 
val sf256 = (fn (t) => t)
val rv272 = (sf256 t)
val rv273 = {sf256 = rv272}
val actuate274 = {t = (#sf256(rv273))}
val sf257 = (fn (rate,t) => ((op +)(t,(random_exponential ((op /)(1000.0,rate))))))
val rv275 = (sf257(rate,t))
val rv276 = {sf257 = rv275}
val actuate277 = {t_next = (#sf257(rv276))}
val union271 = {t_next = (#t_next(actuate277)), t = (#t(actuate274))}

in 
{t_next = (#t_next(union271)), t = (#t(union271)), spikeOutput = (#spikeOutput(transient258))}
end
)
val transientf261 = (fn (transient258,t,h,t_next) => let 
val sf254 = (fn (h,t) => ((op +)(t,h)))
val rv265 = (sf254(h,t))
val rv266 = {sf254 = rv265}
val actuate267 = {t = (#sf254(rv266))}
val sf255 = (fn (t_next) => t_next)
val rv268 = (sf255 t_next)
val rv269 = {sf255 = rv268}
val actuate270 = {t_next = (#sf255(rv269))}
val union264 = {t_next = (#t_next(actuate270)), t = (#t(actuate267))}

in 
{t_next = (#t_next(union264)), t = (#t(union264)), spikeOutput = (#spikeOutput(transient258))}
end
)
val evtest263 = (fn (t_next,t) => let 
val sf253 = (fn (t_next,t) => ((op -)(t,t_next)))
val rv278 = (sf253(t_next,t))
val rv279 = {sf253 = rv278}
val actuate280 = {spikeOutput = (#sf253(rv279))}

in 
{spikeOutput = (#spikeOutput(actuate280))}
end
)

fun ivp_Ext(input as {spikeOutput,rate,t,h,t_next}) = 
let
val transient258 = (evtest263((#t_next(input)),(#t(input))))
val transient259 = (transientf261(transient258,(#t(input)),(#h(input)),(#t_next(input))))
val transient260 = (if (((op >=)((#spikeOutput(transient258)),0.0))) then (transientg262(transient258,(#rate(input)),(#t(input)))) else transient259)

in
transient260
end
end
