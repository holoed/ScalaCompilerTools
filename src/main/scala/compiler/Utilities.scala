package compiler

object StateUtilities {
  // mapM :: (a -> M b) -> [a] -> M [b]
  def mapM[T, V, S](f: T => State[V, S])(xs: List[T]) : State[List[V], S] =
    sequence(xs.map(f))

  // sequence :: [M a] -> M [a]
  def sequence[T, S](xs: List[State[T, S]]) : State[List[T], S] =
    (xs.foldLeft (State.unit[List[T], S](List[T]()))
                 ((macc, mx) => for { acc <- macc
                                          x <- mx }
                                yield acc ++ List(x)))

  def foldM2[T, V, K, S](f : V => K => T => State[T, S])(xs: List[V])(ys:List[K])(acc: T): State[T, S] = {
    (xs, ys) match {
      case (List(), List()) => State.unit(acc)
      case ((x::xs), (y::ys)) => for { faxy <- f (x) (y) (acc)
                                       ret  <- foldM2 (f) (xs) (ys) (faxy) } yield ret
      case (_, List()) => throw new Exception("xs should not be longer than ys.")
      case (List(), _) => throw new Exception("ys should not be longer than xs.")
    }
  }
}
