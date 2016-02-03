
structure Options = 
struct

structure G = GetOpt

exception Error
	      

datatype flag =  Help | Time of real | Timestep of real | Tol of real
                 | Statesample of int | Extsample of int | Evsample of int
                 | Spikerecord of string


fun showflag (Help)       = "Help"
  | showflag (Time x)     = ("Time " ^ (Real.toString x))
  | showflag (Timestep x) = ("Timestep " ^ (Real.toString x))
  | showflag (Tol x)      = ("Tol " ^ (Real.toString x))
  | showflag (Statesample x) = ("Statesample " ^ (Int.toString x))
  | showflag (Extsample x)   = ("Extsample " ^ (Int.toString x))
  | showflag (Evsample x)     = ("Evsample " ^ (Int.toString x))
  | showflag (Spikerecord x)  = ("Spikerecord " ^ x)
		   

val options = 
    [
     {short="h",
      long=["help"],
      desc=G.NoArg (fn() => Help),
      help="show help"},

     {short="t",
      long=["time"],
      desc=G.ReqArg (fn(x) => Time (valOf(Real.fromString x)), "N"),
      help="simulation duration"},

     {short="",
      long=["tol"],
      desc=G.ReqArg (fn(x) => Tol (valOf(Real.fromString x)),"N"),
      help="error tolerance"},

     {short="",
      long=["timestep"],
      desc=G.ReqArg (fn(x) => Timestep (valOf(Real.fromString x)),"N"),
      help="simulation timestep"},

     {short="",
      long=["statesample"],
      desc=G.ReqArg (fn(x) => Statesample (valOf(Int.fromString x)),"N"),
      help="sample size of neurons for state recording"},

     {short="",
      long=["extsample"],
      desc=G.ReqArg (fn(x) => Extsample (valOf(Int.fromString x)),"N"),
      help="sample size of neurons for external input recording"},

     {short="",
      long=["evsample"],
      desc=G.ReqArg (fn(x) => Evsample (valOf(Int.fromString x)),"N"),
      help="sample size of neurons for event recording"},

     {short="s",
      long=["spikerecord"],
      desc=G.ReqArg (fn(x) => Spikerecord (x),"NAME"),
      help="name of population set to be used for spike recording"}


    ]

fun optError (status) (msg) = (status := SOME msg)

fun getopt status = (G.getOpt {argOrder=G.Permute, errFn=optError status,
			       options=options}) 

fun header (progname) = concat ["Usage: ", progname, " [OPTION...] files..."]

fun usage (progname) = G.usageInfo {header=(header progname), options=options}

fun getstate (opts) =

    let
	val O_HELP       = ref false
	val O_TOL        = ref NONE
	val O_TIME       = ref NONE
	val O_TIMESTEP   = ref NONE
	val O_STATESAMPLE   = ref NONE
	val O_EXTSAMPLE     = ref NONE
	val O_EVSAMPLE      = ref NONE
	val O_SPIKERECORD   = ref NONE

	fun getstate' (opt) = 
	    (case opt of 
		 Help          => O_HELP := true	
	       | Tol x         => O_TOL := SOME x
	       | Time x        => O_TIME := SOME x
	       | Timestep x    => O_TIMESTEP := SOME x
	       | Statesample x => O_STATESAMPLE := SOME x
	       | Extsample   x => O_EXTSAMPLE := SOME x
	       | Evsample x    => O_EVSAMPLE := SOME x
	       | Spikerecord x => O_SPIKERECORD := SOME x
            )

	val _ = app getstate' opts

    in {
        is_help=(!O_HELP), 
        is_tol=(!O_TOL), 
        is_time=(!O_TIME),
	is_timestep=(!O_TIMESTEP),
	is_statesample=(!O_STATESAMPLE),
	is_extsample=(!O_EXTSAMPLE),
	is_evsample=(!O_EVSAMPLE),
	is_spikerecord=(!O_SPIKERECORD)
       }
    end

end
