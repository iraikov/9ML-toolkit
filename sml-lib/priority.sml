(* fixed time step event queue *)
functor FixTEventQueue (structure P : PRIORITY
                        type value 
                        val value : P.item -> value) =
struct

    structure PQ = SkewBinomialHeap (P)

    val empty = PQ.empty

    val addEvent  = PQ.insert

    val isEmpty = PQ.isEmpty

    fun nextEvent (f, dflt, pq) =
        case PQ.findMin pq of
            NONE => (dflt, pq)
          | SOME (v) => 
            (f (P.priority v, value v), valOf (PQ.deleteMin pq))

    fun nextEvent' (p, f, dflt, pq) =
        case PQ.findMin pq of
            NONE => (dflt, pq)
          | SOME (v) => 
            (if p (P.priority v, v) 
             then (f (P.priority v, value v), valOf (PQ.deleteMin pq))
             else (dflt, pq))

    fun nextEvents (p, f, pq) =
        let
            fun recur (pq,ax) =
                case PQ.findMin pq of
                    NONE => (ax, pq)
                  | SOME (v) => 
                    (if p (P.priority v, v) 
                     then (let 
                              val v = f (P.priority v, value v)
                          in 
                              recur (valOf (PQ.deleteMin pq), v::ax) 
                          end)
                     else (ax, pq))
        in
            recur (pq, [])
        end



end


functor TEventQueue (structure P : PRIORITY
                     type value 
                     val value : P.item -> value) =
struct

    structure PQ = SkewBinomialHeap (P)

    type priority = P.priority
    type value = value
                                   
    val empty   = PQ.empty

    val isEmpty = PQ.isEmpty

    val merge   = PQ.merge

    fun addEvent (item, pq) =
        PQ.insert (item, pq)


    fun nextEvent (f, dflt, pq) =
        case PQ.findMin pq of
            NONE => (dflt, pq)
          | SOME (v) => 
            (f (P.priority v, v), valOf (PQ.deleteMin pq))


    fun nextEvent' (p, f, dflt, pq) =
        case PQ.findMin pq of
            NONE => (dflt, pq)
          | SOME (v) => 
            (if p (P.priority v, value v) 
             then (f (P.priority v, value v), valOf (PQ.deleteMin pq))
             else (dflt, pq))

    fun nextEvents (p, f, pq) =
        let
            fun recur (pq,ax) =
                case PQ.findMin pq of
                    NONE => (ax, pq)
                  | SOME (v) => 
                    (if p (P.priority v, v) 
                     then (let 
                              val v = f (P.priority v, value v)
                          in 
                              recur (valOf (PQ.deleteMin pq), v::ax) 
                          end)
                     else (ax, pq))
        in
            recur (pq, [])
        end

    fun nextCond (p, f, g, dflt, pq) =
        case PQ.findMin pq of
            NONE => (dflt, pq)
          | SOME (v) => 
            (if p (P.priority v, value v) 
             then (f (P.priority v, value v), valOf (PQ.deleteMin pq))
             else (g (P.priority v, value v), pq))


    val app = PQ.app
    val foldl  = PQ.foldl

end
