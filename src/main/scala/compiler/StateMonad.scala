package compiler

final case class State[+T, S](m : S => (S, T)) {
    def flatMap[U](f : T => State[U, S]) : State[U, S] = State(s => {
      val (s2, x) = m(s)
      f(x) match { case State(n) => n(s2) }
    })

    def map[U](f: T => U) : State[U, S] = this.flatMap(x => State.unit(f(x)))
}

object State {
  // a -> M a
  def unit[T, S](x:T) : State[T, S] = State(s => (s, x))

  // M a
  def getState[S] : State[S, S] = State(s => (s, s))

  // s -> M ()
  def putState[S](s2: S) : State[Unit, S] = State(_ => (s2, ()))

  // M a -> s -> a
  def runState[T, S](m : State[T,S]) (s:S) : T = m match {
    case State(m) => m(s)._2
  }
}
