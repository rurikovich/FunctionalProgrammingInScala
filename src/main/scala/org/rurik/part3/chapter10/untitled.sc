val seq = IndexedSeq(1, 2, 3, 4, 5, 6)

val s = seq.size
seq.slice(0, s / 2)
seq.slice(s / 2, s )
