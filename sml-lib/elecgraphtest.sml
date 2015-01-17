
val seed   = Random.rand (13,17)
val prob   = 0.1
val SA     = SparseMatrix.fromGenerator
                 [10,10] 
                 (fn (i) => (if Real.>= (Random.randReal seed, prob) then 1.0 else 0.0),
                  [4,4], SOME [5,5])

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)


val g = ElecGraph.elecGraph ("test", ([{size=10,localStart=0,globalStart=0}],
                                      [{size=10,localStart=0,globalStart=0}])) SA

val _ = print (ElecGraph.toString  g)

val J = ElecGraph.junctionMatrix ([10,10], [10,10], g)

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("J column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (J,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)
