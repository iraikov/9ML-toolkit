
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
((showReal (#t(input)))^ " " ^ (showReal (#spikeOutput(input)))^ " " ^ (showReal (#t_next(input))))

fun start (tmax,f,initial) =
let
  fun run (input) =
    let val nstate = f input
        val nstate1 = {t_next=(#t_next(nstate)),t=(#t(nstate)),spikeOutput=(#spikeOutput(nstate)),h=(#h(input))}
    in putStrLn (printstate nstate1);
       if (#t nstate)  > tmax
       then (putStrLn "# All done!"; nstate1)
       else (run nstate1)
    end
in
  run (initial)
end
 
val initial = {t_next=(0.0),t=(0.0),spikeOutput=(~1.0),h=(0.1)}
