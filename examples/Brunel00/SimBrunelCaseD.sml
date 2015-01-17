
structure SimBrunelCaseD =
struct

fun putStrLn out str = 
    (TextIO.output (out, str);
     TextIO.output (out, "\n"))
    
fun putStr out str = 
    (TextIO.output (out, str))
    
    
fun showBoolean b = (if b then "1" else "0")
                    
                    
fun showReal n = 
    let open StringCvt
	open Real
    in
	(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
    end


exception Index

fun start (tmax, N, S, D, DQ, Pn, initial_vector, psr_initial_vector, frun, fresponse, ftime, fspikes, finfo, out) =

let
    val sub = Unsafe.Real64Array.sub
    val isub = Unsafe.IntArray.sub
    val update = Unsafe.Real64Array.update

        
    fun netrun (DQ,state_vector,psr_state_vector) =
        let 
            val t = ftime state_vector
            val prio = BrunelCaseD.TEventPriority.delayPriority t

            val (I,DQ') = BrunelCaseD.TEQ.nextEvents 
                                        (fn (prio',_) => prio' <= prio,
                                         fn (_,v) => v,
                                         DQ)
            
            val (W',psr_state_vector_i) = fresponse I psr_state_vector
            
            val state_vector_i = frun W' state_vector

            val t_i = ftime state_vector_i

            val (spike_i, log_spike_i) = fspikes state_vector_i
                
            val _   = finfo (state_vector_i, out)
                              
            val W''  = 
                if (List.null spike_i)
                then NONE
                else SOME 
                         (List.map
                              (fn (W) =>
                                  let
                                      val T = Real64Array.array (N, 0.0)
                                  in
                                      (List.app
                                           (fn (i,nv) => 
                                               let
                                                   val sl = SparseMatrix.slice (W,1,i)
                                               in
                                                   SparseMatrix.sliceAppi 
                                                       (fn (j,x) => update (T,j,Real.+ (Real.* (nv,x), sub(T,j))))
                                                       sl
                                               end)
                                           spike_i;
                                       SparseMatrix.fromTensor [N,1] (RTensor.fromArray ([N,1], T), NONE))
                                  end)
                              S)
                              
            val _ = 
                if not (List.null log_spike_i)
                then (putStr out ((showReal t_i) ^ " ");
                      TensorFile.intListLineWrite out (List.map (fn (i,n) => (i+1)) log_spike_i))
                else (putStrLn out ("# " ^ (showReal t_i)))
                         
                         
            val DQ'' = 
                case W'' of
                    SOME W => 
                    let
                        val DW: (real list * SparseMatrix.matrix list) = (D,W)
                    in
                        (#2(ListPair.foldl
                                (fn(del: real,w: SparseMatrix.matrix,(i,dq)) => 
                                    (i+1, BrunelCaseD.TEQ.addEvent ((Real.+ (t_i,del),(i,w)), dq)))
                                (0, DQ') DW))
                    end
                  | NONE => DQ'
        in
            if t_i  > tmax
            then (putStrLn out "# All done!"; state_vector_i)
            else netrun (DQ'',state_vector_i,psr_state_vector_i)
        end
in
    netrun (DQ,initial_vector,psr_initial_vector)
end

fun timing (action) = 
    let
        val timer = Timer.startCPUTimer ()
        val result = action ()
        val times = Timer.checkCPUTimer timer
    in
        (result, Time.+ (#usr times, #sys times))
    end

fun main (name,args) =
    let
        open BrunelCaseD        fun finfo (_,out) = ()
    in
        (let
            val     _ = putStrLn TextIO.stdOut ("starting fprojection...")
            val (S,t) = timing fprojection
            val     _ = putStrLn TextIO.stdOut ("fprojection took " ^ (Time.toString t) ^ " s")
            val out   = TextIO.openOut (label ^ ".dat")
        in
            List.app (fn (s) => putStrLn out ("# " ^ s))
                     ([
                         label,
                                                  ("order = " ^ (showReal (2500.0))),
                                                  ("Ne = " ^ (showReal (Real.* (4.0, 2500.0)))),
                                                  ("Ni = " ^ (showReal (Real.* (1.0, 2500.0)))),
                                                  ("epsilon = " ^ (showReal (0.1))),
                                                  ("Ce = " ^ (showReal (Real.* (0.1, Real.* (4.0, 2500.0))))),
                                                  ("Ci = " ^ (showReal (Real.* (0.1, Real.* (1.0, 2500.0))))),
                                                  ("Cext = " ^ (showReal (Real.* (0.1, Real.* (4.0, 2500.0))))),
                                                  ("delay = " ^ (showReal (1.5))),
                                                  ("J = " ^ (showReal (0.1))),
                                                  ("g = " ^ (showReal (4.5))),
                                                  ("eta = " ^ (showReal (0.9))),
                                                  ("Je = " ^ (showReal (0.1))),
                                                  ("Ji = " ^ (showReal (Real.~ (Real.* (4.5, 0.1))))),
                                                  ("Jext = " ^ (showReal (0.1))),
                                                  ("theta = " ^ (showReal (20.0))),
                                                  ("tau = " ^ (showReal (20.0))),
                                                  ("nu_thresh = " ^ (showReal (Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0))))),
                                                  ("nu_ext = " ^ (showReal (Real.* (0.9, Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0)))))),
                                                  ("input_rate = " ^ (showReal (Real.* (1000.0, Real.* (0.9, Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0))))))),
                                                  ("timestep = " ^ (showReal (0.2)))
                                              ]);
            let
                val _     = putStrLn TextIO.stdOut ("starting simulation...")
                val (_,t) = timing (fn () => start (1200.0, N, S, D, DQ, Pn, initial, psr_initial, frun, fresponse, ftime, fspikes, finfo, out))
            in
                putStrLn TextIO.stdOut ("simulation took " ^ (Time.toString t) ^ " s")
            end;
            
            TextIO.flushOut (out);
            TextIO.closeOut (out)
        end)
    end
    

end

val _ = let val name = CommandLine.name()
	    val args = CommandLine.arguments()
	    val env  = Posix.ProcEnv.environ()
	in
	    SimBrunelCaseD.main (name, args)
	end

