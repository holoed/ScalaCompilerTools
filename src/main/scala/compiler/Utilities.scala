package compiler

class StateUtilities {
  // mapM :: (a -> M a) -> [a] -> M [a]
  def mapM[T, S](f: T => State[T, S])(xs: List[T]) : State[List[T], S] =
    sequence(xs.map(f))

  // sequence :: [M a] -> M [a]
  def sequence[T, S](xs: List[State[T, S]]) : State[List[T], S] =
    (xs.foldLeft (State.unit[List[T], S](List[T]()))
                 ((macc, mx) => for { acc <- macc
                                          x <- mx }
                                yield acc ++ List(x)))
}
