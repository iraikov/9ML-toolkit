
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
((showReal (#t(input)))^ " " ^ (showReal (#weight(input))))

fun start (tmax,f,initial) =
let
  fun run (input) =
    let val nstate = f input
        val nstate1 = {h=(#h(input)),t=(#t(nstate)),weight=(#weight(nstate))}
    in putStrLn (printstate nstate1);
       if (#t nstate)  > tmax
       then (putStrLn "# All done!"; nstate1)
       else (run nstate1)
    end
in
  run (initial)
end
 
val initial = {h=(0.1),t=(0.0),weight=(Real.~ (Real.* (4.5, 0.1)))}
