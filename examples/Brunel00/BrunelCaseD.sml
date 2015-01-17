
structure BrunelCaseD =
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
      
  fun foldl1 f lst = let val v = List.hd lst
                         val lst' = List.tl lst
                     in
                         List.foldl f v lst'
                     end

  fun fromDiag (m, n, a, dflt) =
      if Index.validShape [m,n]
      then 
          (let 
               val na  = RTensor.Array.length a
               val na' = na-1
               val te  = RTensor.new ([m,n], dflt)
               fun diag (i, j, ia) =
                   let
                       val ia' = 
                           (RTensor.update (te, [i,j], RTensor.Array.sub (a, ia));
                            if ia = na' then 0 else ia+1)
                   in
                       if (i=0) orelse (j=0) 
                       then te
                       else diag (i-1, j-1, ia)
                   end
           in
               diag (m-1, n-1, 0)
           end)
      else 
          raise RTensor.Shape

  val RandomInit = RandomMTZig.fromEntropy

  val ZigInit = RandomMTZig.ztnew
        
  exception Index        

  val label = "BrunelCaseD"
            
  val N = 13500     (* total population size *)


  val order = 2500.0
  val Ne = Real.* (4.0, 2500.0)
  val Ni = Real.* (1.0, 2500.0)
  val epsilon = 0.1
  val Ce = Real.* (0.1, Real.* (4.0, 2500.0))
  val Ci = Real.* (0.1, Real.* (1.0, 2500.0))
  val Cext = Real.* (0.1, Real.* (4.0, 2500.0))
  val delay = 1.5
  val J = 0.1
  val g = 4.5
  val eta = 0.9
  val Je = 0.1
  val Ji = Real.~ (Real.* (4.5, 0.1))
  val Jext = 0.1
  val theta = 20.0
  val tau = 20.0
  val nu_thresh = Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0))
  val nu_ext = Real.* (0.9, Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0)))
  val input_rate = Real.* (1000.0, Real.* (0.9, Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0))))
  val timestep = 0.2

  val h = 0.2
  structure TEventPriority = 
  struct
       type priority     = int
       (* Given a delay (positive real number), compute the priority given a time step *)
       fun delayPriority (delay) = Real.round (Real./ (delay, h))
       
       fun compare (x,y) = Int.compare (x,y)
       type item         = real * (int * SparseMatrix.matrix)
       fun priority (x : item) = delayPriority (#1(x))
  end

  structure TEQ = FixTEventQueue (structure P = TEventPriority
                                  type value = (int * SparseMatrix.matrix)
                                  fun value (x : P.item) = (#2(x)))
  val DQ = TEQ.empty

  (* network propagation delays for each projection *)

  val D: real list = 
      [1.5,1.5,1.5]

  val seed_init = RandomInit() (* seed for randomized initial values *)
  val zt_init   = ZigInit()
  fun random_normal () = RandomMTZig.randNormal(seed_init,zt_init)
  fun random_uniform () = RandomMTZig.randUniform(seed_init)


  val N_Exc = 10000
  val Exc_initial = {Vreset=(10.0),tau_rp=(2.0),theta=(Real.+ (20.0, random_normal ())),tau=(20.0),R=(100.0),spikeOutput=(~1.0),t=(0.0),st117=(false),t_rpend=(0.0),refractoryEnd=(~1.0),V=(random_uniform ()),h=(h),Isyn=(0.0)}
  val Exc_field_vector = 
    Vector.tabulate (N_Exc, fn (i) =>  {theta=(Real.+ (20.0, random_normal ()))})

  val Exc_initial_vector = 
    Vector.tabulate (N_Exc, fn (i) =>  {spikeOutput=(~1.0),t=(0.0),st117=(false),t_rpend=(0.0),refractoryEnd=(~1.0),V=(random_uniform ())})

  val Exc_f = Model_Exc.ivp_E
  fun Exc_run (Wnet,n0) (i,input as { t,st117,refractoryEnd,spikeOutput,V,t_rpend }) =
    let 
        val initial = Exc_initial
        val fieldV = Vector.sub (Exc_field_vector,i)

        val Isyn_i  = 
            case Wnet of 
                SOME W => foldl (fn (W,ax) => Real.+ (RTensor.sub(W,[i+n0,0]), ax)) 0.0 W
              | NONE => 0.0


        (*val _ = putStrLn TextIO.stdOut ("# Exc: t = " ^ (showReal t) ^ " Isyn_i = " ^ (showReal Isyn_i) ^ " V = " ^ (showReal V))*)
        val nstate = Exc_f {spikeOutput=(#spikeOutput(input)),t=(#t(input)),st117=(#st117(input)),t_rpend=(#t_rpend(input)),refractoryEnd=(#refractoryEnd(input)),V=(#V(input)),h=(#h(initial)),Isyn=Isyn_i,Vreset=(#Vreset(initial)),tau_rp=(#tau_rp(initial)),theta=(#theta(fieldV)),tau=(#tau(initial)),R=(#R(initial))} 
        val nstate' = {t=(#t(nstate)),st117=(#st117(nstate)),refractoryEnd=(#refractoryEnd(nstate)),spikeOutput=(#spikeOutput(nstate)),V=(#V(nstate)),t_rpend=(#t_rpend(nstate))} 
    in 
        nstate'
    end


  val N_Inh = 2500
  val Inh_initial = {Vreset=(10.0),tau_rp=(2.0),theta=(Real.+ (20.0, random_normal ())),tau=(20.0),R=(100.0),spikeOutput=(~1.0),t=(0.0),st118=(false),t_rpend=(0.0),refractoryEnd=(~1.0),V=(random_uniform ()),h=(h),Isyn=(0.0)}
  val Inh_field_vector = 
    Vector.tabulate (N_Inh, fn (i) =>  {theta=(Real.+ (20.0, random_normal ()))})

  val Inh_initial_vector = 
    Vector.tabulate (N_Inh, fn (i) =>  {spikeOutput=(~1.0),t=(0.0),st118=(false),t_rpend=(0.0),refractoryEnd=(~1.0),V=(random_uniform ())})

  val Inh_f = Model_Inh.ivp_I
  fun Inh_run (Wnet,n0) (i,input as { t,st118,refractoryEnd,spikeOutput,V,t_rpend }) =
    let 
        val initial = Inh_initial
        val fieldV = Vector.sub (Inh_field_vector,i)

        val Isyn_i  = 
            case Wnet of 
                SOME W => foldl (fn (W,ax) => Real.+ (RTensor.sub(W,[i+n0,0]), ax)) 0.0 W
              | NONE => 0.0


        (*val _ = putStrLn TextIO.stdOut ("# Inh: t = " ^ (showReal t) ^ " Isyn_i = " ^ (showReal Isyn_i) ^ " V = " ^ (showReal V))*)
        val nstate = Inh_f {spikeOutput=(#spikeOutput(input)),t=(#t(input)),st118=(#st118(input)),t_rpend=(#t_rpend(input)),refractoryEnd=(#refractoryEnd(input)),V=(#V(input)),h=(#h(initial)),Isyn=Isyn_i,Vreset=(#Vreset(initial)),tau_rp=(#tau_rp(initial)),theta=(#theta(fieldV)),tau=(#tau(initial)),R=(#R(initial))} 
        val nstate' = {t=(#t(nstate)),st118=(#st118(nstate)),refractoryEnd=(#refractoryEnd(nstate)),spikeOutput=(#spikeOutput(nstate)),V=(#V(nstate)),t_rpend=(#t_rpend(nstate))} 
    in 
        nstate'
    end


  val N_Ext = 1000
  val Ext_initial = {rate=(Real.* (1000.0, Real.* (0.9, Real./ (20.0, Real.* (Real.* (0.1, Real.* (0.1, Real.* (4.0, 2500.0))), 20.0))))),t_next=(0.0),t=(0.0),spikeOutput=(~1.0),h=(h)}

  val Ext_initial_vector = 
    Vector.tabulate (N_Ext, fn (i) =>  {t_next=(0.0),t=(0.0),spikeOutput=(~1.0)})

  val Ext_f = Model_Ext.ivp_Ext
  fun Ext_run (Wnet,n0) (i,input as { t,spikeOutput,t_next }) =
    let 
        val initial = Ext_initial


        (*val _ = putStrLn TextIO.stdOut ("# Ext: t = " ^ (showReal t) ^ " Isyn_i = " ^ (showReal Isyn_i) ^ " V = " ^ (showReal V))*)
        val nstate = Ext_f {t_next=(#t_next(input)),t=(#t(input)),spikeOutput=(#spikeOutput(input)),h=(#h(initial)),rate=(#rate(initial))} 
        val nstate' = {t=(#t(nstate)),spikeOutput=(#spikeOutput(nstate)),t_next=(#t_next(nstate))} 
    in 
        nstate'
    end


  val ExternalPlasticity_initial = {h=(h),t=(0.0),weight=(0.1)}
  val ExcitatoryPlasticity_initial = {h=(h),t=(0.0),weight=(0.1)}
  val InhibitoryPlasticity_initial = {h=(h),t=(0.0),weight=(Real.~ (Real.* (4.5, 0.1)))}




  val RandomUniform_initial = {epsilon=(0.1)}
  val RandomUniform_f = Model_RandomUniform.RandomUniform

    val initial = (
        Exc_initial_vector,
        Inh_initial_vector,
        Ext_initial_vector
    )

    val psr_initial = (
    )


    val Exc_n0 = 0
    val Inh_n0 = 10000
    val Ext_n0 = 12500

                
    val Pn = [
                Exc_n0,
                Inh_n0,
                Ext_n0
            ]
         
    fun frun I
             (
              Exc_state_vector,
              Inh_state_vector,
              Ext_state_vector
 ) =
        let
            val Exc_state_vector' = 
                Vector.mapi (Exc_run (I,Exc_n0))
                            Exc_state_vector

            val Inh_state_vector' = 
                Vector.mapi (Inh_run (I,Inh_n0))
                            Inh_state_vector

            val Ext_state_vector' = 
                Vector.mapi (Ext_run (I,Ext_n0))
                            Ext_state_vector

        in
            (
             Exc_state_vector',
             Inh_state_vector',
             Ext_state_vector'
            ) 
        end

                         
    fun fresponse I
             (
             ) =
        let
            val sub = Unsafe.Real64Array.sub
            val update = Unsafe.Real64Array.update

            val W = [RTensor.new ([N,1],0.0)]
            val A = RTensor.toArray (hd(W))

            val _ = app (fn(_,w) => SparseMatrix.appi (fn([i,_],v) => update (A,i,Real.+ (v, sub(A,i)))) w) I
        in
            (SOME W,
             (
             ))
        end


    fun felec E I 
              (              Exc_state_vector,                            Inh_state_vector,                            Ext_state_vector              ) =
        case E of NONE => I
                | SOME E => 
                  let
                    val update = Unsafe.Real64Array.update
                    val I' = case I of SOME I => I
                                     | NONE => RTensor.new ([N,1],0.0)
                    
                  in
                      SOME I'
                  end
                         
                         
    fun ftime (
              Exc_state_vector,
              Inh_state_vector,
              Ext_state_vector
 ) =
        
        let 
            val { t,st117,refractoryEnd,spikeOutput,V,t_rpend } = Vector.sub (Exc_state_vector,0)
        in t end

    
        
    fun fspikes (
              Exc_state_vector,              Inh_state_vector,              Ext_state_vector ) =

        let
            val Exc_spike_i = 
                Vector.foldri (fn (i,v as { t,st117,refractoryEnd,spikeOutput,V,t_rpend },ax) => 
                               (if (Real.>= (#spikeOutput(v), 0.0)) 
                                then ((i+Exc_n0,1.0))::ax 
                                else ax))
                              [] Exc_state_vector

            val Inh_spike_i = 
                Vector.foldri (fn (i,v as { t,st118,refractoryEnd,spikeOutput,V,t_rpend },ax) => 
                               (if (Real.>= (#spikeOutput(v), 0.0)) 
                                then ((i+Inh_n0,1.0))::ax 
                                else ax))
                              [] Inh_state_vector

            val Ext_spike_i = 
                Vector.foldri (fn (i,v as { t,spikeOutput,t_next },ax) => 
                               (if (Real.>= (#spikeOutput(v), 0.0)) 
                                then ((i+Ext_n0,1.0))::ax 
                                else ax))
                              [] Ext_state_vector


           val ext_spike_i = List.concat ( Ext_spike_i :: [] )

                
           val neuron_spike_i = List.concat [ 
                                 Inh_spike_i,                                 Exc_spike_i                                 ]

            val all_spike_i    = List.concat [neuron_spike_i, ext_spike_i]
        in
            (all_spike_i, neuron_spike_i)
        end

















           
           
    fun fprojection () =
        
        (let
             
             val _ = putStrLn TextIO.stdOut "constructing External"

             val Pr_External = let
                                  val weight = #weight(ExternalPlasticity_initial)
                               in
                                  SparseMatrix.fromTensorList [N,N]
                                  [
      {offset=[10000,12500],
                                     tensor=(RTensor.*> weight (RTensor.new ([2500,1000],1.0))),
                                     sparse=false},      {offset=[0,12500],
                                     tensor=(RTensor.*> weight (RTensor.new ([10000,1000],1.0))),
                                     sparse=false}                                     ]
                                end
             val _ = putStrLn TextIO.stdOut "constructing Excitation"

             val Pr_Excitation = let
                                  val weight  = #weight(ExcitatoryPlasticity_initial)
                               in
                                  SparseMatrix.fromGeneratorList [N,N]
                                  [
      {offset=[10000,0],
                                       fshape=[2500,10000],
                                       f=(fn (i) => Real.* (weight, #connection(RandomUniform_f RandomUniform_initial) ))},      {offset=[0,0],
                                       fshape=[10000,10000],
                                       f=(fn (i) => Real.* (weight, #connection(RandomUniform_f RandomUniform_initial) ))}                                     ]
                               end
             val _ = putStrLn TextIO.stdOut "constructing Inhibition"

             val Pr_Inhibition = let
                                  val weight  = #weight(InhibitoryPlasticity_initial)
                               in
                                  SparseMatrix.fromGeneratorList [N,N]
                                  [
      {offset=[10000,10000],
                                       fshape=[2500,2500],
                                       f=(fn (i) => Real.* (weight, #connection(RandomUniform_f RandomUniform_initial) ))},      {offset=[0,10000],
                                       fshape=[10000,2500],
                                       f=(fn (i) => Real.* (weight, #connection(RandomUniform_f RandomUniform_initial) ))}                                     ]
                               end
                
       
             val S = foldl1 SparseMatrix.insert
                     ([ 
                                                                     Pr_External,                                                                                            Pr_Excitation,                                                                                            Pr_Inhibition                                                                   ])


             val Elst =  
                      [ 
                                                                                              ]

             val E = if List.null Elst then NONE else SOME Elst

             in
              ([
               S
              ])
             end)

end
        

