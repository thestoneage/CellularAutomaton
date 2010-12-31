object Program {
  def main(args: Array[String] = null) {
    ElementaryCellularAutomaton.evolveFrom(
      ElementaryCellularAutomaton(184, List(false, true, false)))
      .take(10)
      .foreach(println _)
  }
}

// vim: set ts=2 sw=2 et:
