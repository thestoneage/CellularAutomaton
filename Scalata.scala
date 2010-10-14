import scala.collection.immutable.Stream

class ElementaryCellularAutomata(rule:Int, initial:List[Boolean]) {
    assert(rule < 256)

    def rule(slice:List[Boolean]):Boolean = {
        slice match {
            case List(false, false, false) => (rule & 0x01) == 0x01
            case List(false, false, true)  => (rule & 0x02) == 0x02
            case List(false, true,  false) => (rule & 0x04) == 0x04
            case List(false, true,  true)  => (rule & 0x08) == 0x08
            case List(true,  false, false) => (rule & 0x10) == 0x10
            case List(true,  false, true)  => (rule & 0x20) == 0x20
            case List(true,  true,  false) => (rule & 0x40) == 0x40
            case List(true,  true,  true)  => (rule & 0x80) == 0x80
        }
    }

    def next = {
        val lst = ((initial.last :: initial) :+ initial.head).sliding(3).map(x => rule(x)).toList
        new ElementaryCellularAutomata(rule, lst)
    }

    override def toString() = {
        initial.foldLeft("")((acc:String, x:Boolean) => if (x) acc ++ "+" else acc ++ "-")
    }

}

def from(automaton:ElementaryCellularAutomata):Stream[ElementaryCellularAutomata] = {
    Stream.cons(automaton, from(automaton.next))
}


// vim: set ts=4 sw=4 et:
