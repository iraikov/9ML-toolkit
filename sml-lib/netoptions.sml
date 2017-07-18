
structure Options = 
struct

structure G = GetOpt

exception Error
	      

datatype flag =  Help | Time of real | Timestep of real | Tol of real
                 | Statesample of int | Extsample of int | Evsample of int
                 | Spikerecord of string | Prjrecord | Logperiod of real
                 | Spikeout of string | Stateprefix of string
                 | Eventprefix of string | Extprefix of string
                 | Prjprefix of string
                 | CellRandomseeds of int list
                 | PrjRandomseeds of int list

fun list2str elem2str lst =
  String.concatWith "," (List.map elem2str lst) 
                    
fun showflag (Help)       = "Help"
  | showflag (Time x)     = ("Time " ^ (Real.toString x))
  | showflag (Timestep x) = ("Timestep " ^ (Real.toString x))
  | showflag (Logperiod x) = ("Logperiod " ^ (Real.toString x))
  | showflag (Tol x)      = ("Tol " ^ (Real.toString x))
  | showflag (Statesample x) = ("Statesample " ^ (Int.toString x))
  | showflag (Extsample x)   = ("Extsample " ^ (Int.toString x))
  | showflag (Evsample x)     = ("Evsample " ^ (Int.toString x))
  | showflag (Spikerecord x)  = ("Spikerecord " ^ x)
  | showflag (Prjrecord)  = ("Prjrecord")
  | showflag (Spikeout x)  = ("Spikeout " ^ x)
  | showflag (Eventprefix x)  = ("Eventprefix " ^ x)
  | showflag (Stateprefix x)  = ("Stateprefix " ^ x)
  | showflag (Extprefix x)  = ("Extprefix " ^ x)
  | showflag (Prjprefix x)  = ("Prjprefix " ^ x)
  | showflag (CellRandomseeds x)  = ("CellRandomseeds " ^ (list2str Int.toString x))
  | showflag (PrjRandomseeds x)  = ("PrjRandomseeds " ^ (list2str Int.toString x))
		   

fun s2r s = Real.fromString (String.map (fn #"-" => #"~" | c => c) s)
fun s2i s = s2i (String.map (fn #"-" => #"~" | c => c) s)

val options = 
    [
     {short="h",
      long=["help"],
      desc=G.NoArg (fn() => Help),
      help="show help"},

     {short="d",
      long=["duration"],
      desc=G.ReqArg (fn(x) => Time (valOf(s2r x)), "N"),
      help="simulation duration"},

     {short="",
      long=["tol"],
      desc=G.ReqArg (fn(x) => Tol (valOf(s2r x)),"N"),
      help="error tolerance"},

     {short="",
      long=["timestep"],
      desc=G.ReqArg (fn(x) => Timestep (valOf(s2r x)),"N"),
      help="simulation timestep"},

     {short="",
      long=["logperiod"],
      desc=G.ReqArg (fn(x) => Logperiod (valOf(s2r x)),"N"),
      help="write out spike times and state information every N ms of simulated time"},

     {short="",
      long=["statesample"],
      desc=G.ReqArg (fn(x) => Statesample (valOf(s2i x)),"N"),
      help="sample size of neurons for state recording"},

     {short="",
      long=["extsample"],
      desc=G.ReqArg (fn(x) => Extsample (valOf(s2i x)),"N"),
      help="sample size of neurons for external input recording"},

     {short="",
      long=["evsample"],
      desc=G.ReqArg (fn(x) => Evsample (valOf(s2i x)),"N"),
      help="sample size of neurons for event recording"},

     {short="s",
      long=["spikerecord"],
      desc=G.ReqArg (fn(x) => Spikerecord (x),"NAME"),
      help="name of population set to be used for spike recording"},

     {short="",
      long=["prjrecord"],
      desc=G.NoArg (fn() => Prjrecord),
      help="record projections to files"},

     {short="",
      long=["cell-randomseeds"],
      desc=G.ReqArg (fn(x) =>
                        let
                            val lst = List.mapPartial s2i (String.tokens (fn(c) => c=(#",")) x)
                        in
                            CellRandomseeds lst
                        end,
                     "SEED1,...,SEEDN"),
      help="random seeds for spike generation per each population"},

     {short="",
      long=["prj-randomseeds"],
      desc=G.ReqArg (fn(x) =>
                        let
                            val lst = List.mapPartial s2i (String.tokens (fn(c) => c=(#",")) x)
                        in
                            PrjRandomseeds lst
                        end,
                     "SEED1,...,SEEDN"),
      help="random seeds for connectivity construction per each projection"},

     {short="",
      long=["spikeout"],
      desc=G.ReqArg (fn(x) => Spikeout (x),"PATH"),
      help="path to file name used for spike recording"},

     {short="",
      long=["stateprefix"],
      desc=G.ReqArg (fn(x) => Stateprefix (x),"PATH"),
      help="prefix for file names used for state recording"},

     {short="",
      long=["eventprefix"],
      desc=G.ReqArg (fn(x) => Eventprefix (x),"PATH"),
      help="prefix for file names used for event recording"},

     {short="",
      long=["extprefix"],
      desc=G.ReqArg (fn(x) => Extprefix (x),"PATH"),
      help="prefix for file name used for external input recording"},

     {short="",
      long=["prjprefix"],
      desc=G.ReqArg (fn(x) => Prjprefix (x),"PREFIX"),
      help="prefix for file name used for printing projections"}


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
	val O_LOGPERIOD       = ref NONE
	val O_STATESAMPLE     = ref NONE
	val O_EXTSAMPLE       = ref NONE
	val O_EVSAMPLE        = ref NONE
	val O_SPIKERECORD     = ref NONE
	val O_PRJRECORD       = ref false
	val O_SPIKEOUT        = ref NONE
	val O_STATEPREFIX     = ref NONE
	val O_EXTPREFIX       = ref NONE
	val O_EVENTPREFIX     = ref NONE
	val O_PRJPREFIX       = ref NONE
	val O_CELL_RANDOMSEEDS = ref NONE
	val O_PRJ_RANDOMSEEDS  = ref NONE

	fun getstate' (opt) = 
	    (case opt of 
		 Help           => O_HELP := true	
	       | Tol x          => O_TOL := SOME x
	       | Time x         => O_TIME := SOME x
	       | Timestep x     => O_TIMESTEP := SOME x
	       | Logperiod x    => O_LOGPERIOD := SOME x
	       | Statesample x  => O_STATESAMPLE := SOME x
	       | Extsample   x  => O_EXTSAMPLE := SOME x
	       | Evsample x     => O_EVSAMPLE := SOME x
	       | Spikerecord x  => O_SPIKERECORD := SOME x
	       | Prjrecord      => O_PRJRECORD := true
	       | Spikeout x     => O_SPIKEOUT := SOME x
	       | Stateprefix x  => O_STATEPREFIX := SOME x
	       | Extprefix x    => O_EXTPREFIX := SOME x
	       | Eventprefix x  => O_EVENTPREFIX := SOME x
	       | Prjprefix x    => O_PRJPREFIX := SOME x
	       | CellRandomseeds x => O_CELL_RANDOMSEEDS := SOME x
	       | PrjRandomseeds x  => O_PRJ_RANDOMSEEDS := SOME x
            )

	val _ = app getstate' opts

    in {
        is_help=(!O_HELP), 
        is_time=(!O_TIME),
	is_timestep=(!O_TIMESTEP),
        is_tol=(!O_TOL), 
        is_logperiod=(!O_LOGPERIOD), 
	is_statesample=(!O_STATESAMPLE),
	is_extsample=(!O_EXTSAMPLE),
	is_evsample=(!O_EVSAMPLE),
	is_spikerecord=(!O_SPIKERECORD),
	is_prjrecord=(!O_PRJRECORD),
	is_spikeout=(!O_SPIKEOUT),
	is_stateprefix=(!O_STATEPREFIX),
	is_eventprefix=(!O_EVENTPREFIX),
	is_extprefix=(!O_EXTPREFIX),
	is_prjprefix=(!O_PRJPREFIX),
	is_cell_randomseeds=(!O_CELL_RANDOMSEEDS),
	is_prj_randomseeds=(!O_PRJ_RANDOMSEEDS)
       }
    end

end
