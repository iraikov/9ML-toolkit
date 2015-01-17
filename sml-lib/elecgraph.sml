(* Electrical synapses graph structure *)


signature ELEC_GRAPH =
sig
    type index = int
    type rangemap = {size: int, localStart: int, globalStart: int} list

    val elecGraph : (string * (rangemap * rangemap)) -> SparseMatrix.matrix -> 
		    (index, real, unit) Graph.graph
		   
    val junctionMatrix : (SparseMatrix.index * SparseMatrix.index * (index, real, unit) Graph.graph) -> SparseMatrix.matrix

    val toString : ((index, real, unit) Graph.graph) -> string
end

structure ElecGraph: ELEC_GRAPH =
struct

type index    = int
type rangemap = {size: int, localStart: int, globalStart: int} list

exception Index

fun elecGraph (modelname, (srangemap,trangemap)) S =
    let
        fun mapIndex m i = case List.find (fn {size,localStart,globalStart} => 
                                              (i >= localStart andalso i < localStart+size)) m of
                               SOME ({size,localStart,globalStart}) => globalStart + (i-localStart)
                             | NONE => raise Index
        
        val [N,M] = SparseMatrix.shape S
        
	val G as Graph.GRAPH g = 
            DirectedGraph.graph(modelname,(),N) :
	    (index,real,unit) Graph.graph 

        val add_node = #add_node g
        val add_edge = #add_edge g

        val _ = Loop.app
                    (0, N,
                     (fn (idx) => 
                         let
                             val sl  = SparseMatrix.slice (S,1,idx)
                             val gid = mapIndex srangemap idx 
                             val _   = add_node (gid,idx)
                         in
                             SparseMatrix.sliceAppi 
                                 (fn (t,v) => 
                                     let 
                                         val tgid = mapIndex trangemap t 
                                     in
                                         add_node (tgid,t);  
                                         add_edge (gid, tgid, v)
                                     end)
                                 sl
                         end))

    in
	G
    end


fun junctionMatrix (globalShape,localShape,Graph.GRAPH g) =
    let
        fun nodeCoeffs n = 
            let
                val out  = (#out_edges g) n
                val self = Real.* (~1.0, foldl (fn ((s,t,v),ax) => Real.+(v,ax)) 0.0 out)
            in
                (n, (n,self) :: (map (fn (s,t,v) => (t,v)) out))
            end

        val lst = ref []
            
    in
        ((#forall_nodes g) (fn (n,_) => (lst := ((nodeCoeffs n) :: !lst)));
         SparseMatrix.fromList globalShape (!lst,localShape,NONE))
    end

fun node2str (gid) = (Int.toString gid)
fun edgelabel2str (v) = (Real.toString v)

fun toString (Graph.GRAPH G) =
   let fun showEdges es = 
          String.concat(
             map (fn (i,j,v) => Int.toString i^" -> "^Int.toString j ^ " (" ^ (edgelabel2str v) ^ ")\n") es)

       fun showNodesLabels ns = 
          (String.concatWith "\n" 
			     (map (fn (n,l) => (node2str l) ^ " : " ^ (Int.toString n)) ns))^"\n"
       fun showNodes ns = 
          (String.concatWith ", " 
			     (map (fn (n) => (Int.toString n)) ns))^"\n"
   in     
       #name G ^ "\n" ^
       "nodes: "^showNodesLabels (#nodes G ())^
       "edges:\n"^showEdges(#edges G ())^
       "entry edges:\n"^
           showEdges(List.concat(map (#entry_edges G o #1) (#nodes G ())))^ 
       "exit edges:\n"^
           showEdges(List.concat(map (#exit_edges G o #1) (#nodes G ())))^ 
       "entries: "^showNodes(#entries G ())^
       "exits: "^showNodes(#exits G ())
   end
    


end
