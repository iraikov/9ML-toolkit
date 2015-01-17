
fun putStrLn str = 
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))
    
fun putStr str = 
    (TextIO.output (TextIO.stdOut, str))
    
fun showBoolean b = (if b then "1" else "0")

fun showReal n = 
    let open StringCvt
	open Real
    in
	(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
    end
fun printstate (input) = 
((showReal (#t(input)))^ " " ^ (showBoolean (#st118(input)))^ " " ^ (showReal (#refractoryEnd(input)))^ " " ^ (showReal (#spikeOutput(input)))^ " " ^ (showReal (#V(input)))^ " " ^ (showReal (#t_rpend(input))))

fun start (tmax,f,initial) =
let
  fun run (input) =
    let val nstate = f input
        val nstate1 = {spikeOutput=(#spikeOutput(nstate)),t=(#t(nstate)),st118=(#st118(nstate)),t_rpend=(#t_rpend(nstate)),refractoryEnd=(#refractoryEnd(nstate)),V=(#V(nstate)),h=(#h(input)),Isyn=(#Isyn(input))}
    in putStrLn (printstate nstate1);
       if (#t nstate)  > tmax
       then (putStrLn "# All done!"; nstate1)
       else (run nstate1)
    end
in
  run (initial)
end
 
val initial = {spikeOutput=(~1.0),t=(0.0),st118=(false),t_rpend=(0.0),refractoryEnd=(~1.0),V=(random_uniform ()),h=(0.1),Isyn=(0.0)}
