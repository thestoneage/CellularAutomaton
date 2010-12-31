import scala.collection.immutable.Stream

object ElementaryCellularAutomaton {
  def apply(rule:Short, initial:List[Boolean]) = new ElementaryCellularAutomaton(rule, initial)

  def evolveFrom(automaton:ElementaryCellularAutomaton):Stream[ElementaryCellularAutomaton] =
    automaton #:: evolveFrom(automaton.next)
}

class ElementaryCellularAutomaton(rule:Short, initial:List[Boolean]) {
  assert(rule < 256)

  def rule(slice:List[Boolean]):Boolean = slice match {
    case List(false, false, false) => (rule & 0x01) == 0x01
    case List(false, false, true ) => (rule & 0x02) == 0x02
    case List(false, true,  false) => (rule & 0x04) == 0x04
    case List(false, true,  true ) => (rule & 0x08) == 0x08
    case List(true,  false, false) => (rule & 0x10) == 0x10
    case List(true,  false, true ) => (rule & 0x20) == 0x20
    case List(true,  true,  false) => (rule & 0x40) == 0x40
    case List(true,  true,  true ) => (rule & 0x80) == 0x80
  }

  def next = {
    val lst = ((initial.last :: initial) :+ initial.head).sliding(3).map(rule _).toList
    ElementaryCellularAutomaton(rule, lst)
  }

  def evolutionStream = this #:: evolve(next)

  override def toString() = initial.map(if (_) "+" else "-").mkString

  private def evolve(automaton:ElementaryCellularAutomaton):Stream[ElementaryCellularAutomaton] =
    this #:: evolve(next)
}

// vim: set ts=2 sw=2 et:
