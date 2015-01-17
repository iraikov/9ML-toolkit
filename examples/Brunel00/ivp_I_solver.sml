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

val trstm206 = (TRC((fn (st118,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t) => let 
val Vprime191 = (fn (tau,Isyn,R,V) => ((op /)(((op +)((neg V),((op *)(R,Isyn)))),tau)))
val evcompute210 = (fn (theta,V) => let 
val sf186 = (fn (theta,V) => ((op -)(V,theta)))
val rv211 = (sf186(theta,V))
val rv212 = {sf186 = rv211}
val actuate213 = {spikeOutput = (#sf186(rv212))}

in 
{spikeOutput = (#spikeOutput(actuate213))}
end
)
val evtest209 = (fn (yvec) => (#spikeOutput((evcompute210(theta,Unsafe.Vector.sub (yvec, 0))))))
val dfn194 = (fn (t,yvec) => let 
val V = Unsafe.Vector.sub (yvec, 0)

in 
(Vector.fromList [(Vprime191(tau,Isyn,R,V))])
end
)
val integral192 = (eintegral(dfn194,t,(Vector.fromList [V]),evtest209,h,0))
val ysn214 = (#ysn(integral192))
val xn215 = (#xn(integral192))
val integral193 = {t = xn215, V_t_h188_189 = Unsafe.Vector.sub (ysn214, 0)}
val actuate216 = {t = (#t(integral193)), V = (#V_t_h188_189(integral193))}
val etest228 = (fn (theta,V) => let 
val sf202 = (fn (theta,V) => ((op -)(V,theta)))
val rv225 = (sf202(theta,V))
val rv226 = {sf202 = rv225}
val actuate227 = {spikeOutput = (#sf202(rv226))}

in 
(#spikeOutput(actuate227))
end
)

in 
(TRSA {refractoryEnd = ~1.0, spikeOutput = (etest228(theta,(#V(actuate216)))), V = (#V(actuate216)), t = (#t(integral193)), V_t_h188_189 = (#V_t_h188_189(integral193))})
end
),(fn (st118,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t) => let 
val Vprime198 = (fn () => 0.0)
val evcompute218 = (fn (t_rpend,t) => let 
val sf187 = (fn (t_rpend,t) => ((op -)(t,t_rpend)))
val rv219 = (sf187(t_rpend,t))
val rv220 = {sf187 = rv219}
val actuate221 = {refractoryEnd = (#sf187(rv220))}

in 
{refractoryEnd = (#refractoryEnd(actuate221))}
end
)
val evtest217 = (fn (yvec) => (#refractoryEnd((evcompute218(t_rpend,t)))))
val dfn201 = (fn (t,yvec) => let 
val V = Unsafe.Vector.sub (yvec, 0)

in 
(Vector.fromList [Vprime198()])
end
)
val integral199 = (eintegral(dfn201,t,(Vector.fromList [V]),evtest217,h,1))
val ysn222 = (#ysn(integral199))
val xn223 = (#xn(integral199))
val integral200 = {t = xn223, V_t_h195_196 = Unsafe.Vector.sub (ysn222, 0)}
val actuate224 = {t = (#t(integral200)), V = (#V_t_h195_196(integral200))}
val ektest232 = (fn (t_rpend,t) => let 
val sf203 = (fn (t_rpend,t) => ((op -)(t,t_rpend)))
val rv229 = (sf203(t_rpend,t))
val rv230 = {sf203 = rv229}
val actuate231 = {refractoryEnd = (#sf203(rv230))}

in 
(#refractoryEnd(actuate231))
end
)

in 
(TRSB {spikeOutput = ~1.0, refractoryEnd = (ektest232(t_rpend,(#t(integral200)))), V = (#V(actuate224)), t = (#t(integral200)), V_t_h195_196 = (#V_t_h195_196(integral200))})
end
),(fn (x) => (tsCase((fn (x) => (#spikeOutput(x))),(fn (x) => (#refractoryEnd(x))),x)))))
val blender208 = (fn (trstm206,st118,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t) => let 
val f = (trfOf trstm206)
val fk = (trfkOf trstm206)
val e = (treOf trstm206)
val fv = (if (st118) then (fk(st118,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t)) else (f(st118,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t)))
val trp = ((op >=)((e fv),0.0))
val st118 = (if (trp) then (not st118) else st118)

in 
if ((not trp)) 
then (if (st118) then (tsCase((fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv)) else (tsCase((fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv)))
else if (st118) 
then (tsCase((fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv))
else (tsCase((fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),(fn (x) => {st118 = st118, V = (#V(x)), t = (#t(x)), refractoryEnd = (#refractoryEnd(x)), spikeOutput = (#spikeOutput(x))}),fv))


end
)

fun ivp_I(input as {Vreset,tau_rp,st118,refractoryEnd,spikeOutput,R,Isyn,tau,h,V,theta,t_rpend,t}) = 
let
val trv207 = (blender208(trstm206,(#st118(input)),(#refractoryEnd(input)),(#spikeOutput(input)),(#R(input)),(#Isyn(input)),(#tau(input)),(#h(input)),(#V(input)),(#theta(input)),(#t_rpend(input)),(#t(input))))
val onf241 = (fn (t,tau_rp,Vreset) => let 
val sf204 = (fn (tau_rp,t) => ((op +)(t,tau_rp)))
val rv234 = (sf204((#tau_rp(input)),(#t(trv207))))
val rv235 = {sf204 = rv234}
val actuate236 = {t_rpend = (#sf204(rv235))}
val sf205 = (fn (Vreset) => Vreset)
val rv237 = (sf205 (#Vreset(input)))
val rv238 = {sf205 = rv237}
val actuate239 = {V = (#sf205(rv238))}
val union233 = {V = (#V(actuate239)), t_rpend = (#t_rpend(actuate236))}

in 
{spikeOutput = (#spikeOutput(trv207)), t_rpend = (#t_rpend(union233)), V = (#V(union233))}
end
)
val onrv240 = (if (((op >=)((#spikeOutput(trv207)),0.0))) then (onf241((#t(trv207)),(#tau_rp(input)),(#Vreset(input)))) else {spikeOutput = (#spikeOutput(trv207)), t_rpend = (#t_rpend(input)), V = (#V(trv207))})
val sequence242 = {t = (#t(trv207)), st118 = (#st118(trv207)), spikeOutput = (#spikeOutput(trv207)), refractoryEnd = (#refractoryEnd(trv207)), t_rpend = (#t_rpend(onrv240)), V = (#V(onrv240))}

in
sequence242
end
end
