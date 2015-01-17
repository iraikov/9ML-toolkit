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

val trstm139 = (TRC((fn (st117,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t) => let 
val Vprime124 = (fn (tau,Isyn,R,V) => ((op /)(((op +)((neg V),((op *)(R,Isyn)))),tau)))
val evcompute143 = (fn (theta,V) => let 
val sf119 = (fn (theta,V) => ((op -)(V,theta)))
val rv144 = (sf119(theta,V))
val rv145 = {sf119 = rv144}
val actuate146 = {spikeOutput = (#sf119(rv145))}

in 
{spikeOutput = (#spikeOutput(actuate146))}
end
)
val evtest142 = (fn (yvec) => (#spikeOutput((evcompute143(theta,Unsafe.Vector.sub (yvec, 0))))))
val dfn127 = (fn (t,yvec) => let 
val V = Unsafe.Vector.sub (yvec, 0)

in 
(Vector.fromList [(Vprime124(tau,Isyn,R,V))])
end
)
val integral125 = (eintegral(dfn127,t,(Vector.fromList [V]),evtest142,h,0))
val ysn147 = (#ysn(integral125))
val xn148 = (#xn(integral125))
val integral126 = {t = xn148, V_t_h121_122 = Unsafe.Vector.sub (ysn147, 0)}
val actuate149 = {t = (#t(integral126)), V = (#V_t_h121_122(integral126))}
val etest161 = (fn (theta,V) => let 
val sf135 = (fn (theta,V) => ((op -)(V,theta)))
val rv158 = (sf135(theta,V))
val rv159 = {sf135 = rv158}
val actuate160 = {spikeOutput = (#sf135(rv159))}

in 
(#spikeOutput(actuate160))
end
)

in 
(TRSA {refractoryEnd = ~1.0, spikeOutput = (etest161(theta,(#V(actuate149)))), V = (#V(actuate149)), t = (#t(integral126)), V_t_h121_122 = (#V_t_h121_122(integral126))})
end
),(fn (st117,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t) => let 
val Vprime131 = (fn () => 0.0)
val evcompute151 = (fn (t_rpend,t) => let 
val sf120 = (fn (t_rpend,t) => ((op -)(t,t_rpend)))
val rv152 = (sf120(t_rpend,t))
val rv153 = {sf120 = rv152}
val actuate154 = {refractoryEnd = (#sf120(rv153))}

in 
{refractoryEnd = (#refractoryEnd(actuate154))}
end
)
val evtest150 = (fn (yvec) => (#refractoryEnd((evcompute151(t_rpend,t)))))
val dfn134 = (fn (t,yvec) => let 
val V = Unsafe.Vector.sub (yvec, 0)

in 
(Vector.fromList [Vprime131()])
end
)
val integral132 = (eintegral(dfn134,t,(Vector.fromList [V]),evtest150,h,1))
val ysn155 = (#ysn(integral132))
val xn156 = (#xn(integral132))
val integral133 = {t = xn156, V_t_h128_129 = Unsafe.Vector.sub (ysn155, 0)}
val actuate157 = {t = (#t(integral133)), V = (#V_t_h128_129(integral133))}
val ektest165 = (fn (t_rpend,t) => let 
val sf136 = (fn (t_rpend,t) => ((op -)(t,t_rpend)))
val rv162 = (sf136(t_rpend,t))
val rv163 = {sf136 = rv162}
val actuate164 = {refractoryEnd = (#sf136(rv163))}

in 
(#refractoryEnd(actuate164))
end
)

in 
(TRSB {spikeOutput = ~1.0, refractoryEnd = (ektest165(t_rpend,(#t(integral133)))), V = (#V(actuate157)), t = (#t(integral133)), V_t_h128_129 = (#V_t_h128_129(integral133))})
end
),(fn (x) => (tsCase((fn (x) => (#spikeOutput(x))),(fn (x) => (#refractoryEnd(x))),x)))))
val blender141 = (fn (trstm139,st117,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t) => let 
val f = (trfOf trstm139)
val fk = (trfkOf trstm139)
val e = (treOf trstm139)
val fv = (if (st117) then (fk(st117,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t)) else (f(st117,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t)))
val trp = ((op >=)((e fv),0.0))
val st117 = (if (trp) then (not st117) else st117)

in 
if ((not trp)) 
then (if (st117) then (tsCase((fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv)) else (tsCase((fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv)))
else if (st117) 
then (tsCase((fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv))
else (tsCase((fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st117 = st117, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv))


end
)

fun ivp_E(input as {Vreset,tau_rp,st117,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t}) = 
let
val trv140 = (blender141(trstm139,(#st117(input)),(#refractoryEnd(input)),(#spikeOutput(input)),(#R(input)),(#Isyn(input)),(#tau(input)),(#h(input)),(#V(input)),(#theta(input)),(#t_rpend(input)),(#t(input))))
val onf174 = (fn (t,tau_rp,Vreset) => let 
val sf137 = (fn (tau_rp,t) => ((op +)(t,tau_rp)))
val rv167 = (sf137((#tau_rp(input)),(#t(trv140))))
val rv168 = {sf137 = rv167}
val actuate169 = {t_rpend = (#sf137(rv168))}
val sf138 = (fn (Vreset) => Vreset)
val rv170 = (sf138 (#Vreset(input)))
val rv171 = {sf138 = rv170}
val actuate172 = {V = (#sf138(rv171))}
val union166 = {V = (#V(actuate172)), t_rpend = (#t_rpend(actuate169))}

in 
{spikeOutput = (#spikeOutput(trv140)), t_rpend = (#t_rpend(union166)), V = (#V(union166))}
end
)
val onrv173 = (if (((op >=)((#spikeOutput(trv140)),0.0))) then (onf174((#t(trv140)),(#tau_rp(input)),(#Vreset(input)))) else {spikeOutput = (#spikeOutput(trv140)), t_rpend = (#t_rpend(input)), V = (#V(trv140))})
val sequence175 = {t = (#t(trv140)), st117 = (#st117(trv140)), spikeOutput = (#spikeOutput(trv140)), refractoryEnd = (#refractoryEnd(trv140)), t_rpend = (#t_rpend(onrv173)), V = (#V(onrv173))}

in
sequence175
end
end
