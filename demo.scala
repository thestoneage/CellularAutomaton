object demo {
  def main(args: Array[String] = null) {
    val (automaton, initialState, iterations) = args match {
      case Array(a, i, r) => (a.toShort, List(i.map(_ != '0'):_*), r.toInt)
      case Array(a, i) => (a.toShort, List(i.map(_ != '0'):_*), 100)
      case _ => (184:Short, List(false, true, false), 100)
    }
    ElementaryCellularAutomaton.evolveFrom(
      ElementaryCellularAutomaton(automaton, initialState))
      .take(iterations)
      .foreach(println _)
  }
}

// vim: set ts=2 sw=2 et:
