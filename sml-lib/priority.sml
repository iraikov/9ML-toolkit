(* fixed time step event queue *)
functor FixTEventQueue (structure P : PRIORITY
                        type value 
                        val value : P.item -> value) =
struct

    structure PQ = SkewBinomialHeap (P)

    type value = value

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

    fun nextEventsApp (f, pq) =
        let
            fun recur (pq) =
                case PQ.findMin pq of
                    NONE => ()
                  | SOME (v) => 
                    (f (P.priority v, value v);
                     recur (valOf (PQ.deleteMin pq)))
        in
            recur pq
        end

    fun nextEventsPmap (p, f, pq) =
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

    fun nextEventsFold (p, f, init, pq) =
        let
            fun recur (pq,ax) =
                case PQ.findMin pq of
                    NONE => (ax, pq)
                  | SOME (v) => 
                    (if p (P.priority v, v) 
                     then (let 
                              val ax' = f (P.priority v, value v, ax)
                          in 
                              recur (valOf (PQ.deleteMin pq), ax') 
                          end)
                     else (ax, pq))
        in
            recur (pq, init)
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

    val findMin = PQ.findMin

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

    fun nextEventsApp (f, pq) =
        let
            fun recur (pq) =
                case PQ.findMin pq of
                    NONE => ()
                  | SOME (v) => 
                    (f (P.priority v, value v);
                     recur (valOf (PQ.deleteMin pq)))
        in
            recur pq
        end

    fun nextEventsPmap (p, f, pq) =
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

    fun nextEventsFold (p, f, init, pq) =
        let
            fun recur (pq,ax) =
                case PQ.findMin pq of
                    NONE => (ax, pq)
                  | SOME (v) => 
                    (if p (P.priority v, v) 
                     then (let 
                              val ax' = f (P.priority v, value v, ax)
                          in 
                              recur (valOf (PQ.deleteMin pq), ax') 
                          end)
                     else (ax, pq))
        in
            recur (pq, init)
        end

end
