
class ElementaryCellularAutomata(rule:Int, initial:List[Boolean]) {
    assert(number < 256)

    def rule(slice:List[Boolean]):Boolean = {
        slice match {
            case List(false, false, false) => (number & 0x01) == 0x01
            case List(false, false, true)  => (number & 0x02) == 0x02
            case List(false, true,  false) => (number & 0x04) == 0x04
            case List(false, true,  true)  => (number & 0x08) == 0x08
            case List(true,  false, false) => (number & 0x10) == 0x10
            case List(true,  false, true)  => (number & 0x20) == 0x20
            case List(true,  true,  false) => (number & 0x40) == 0x40
            case List(true,  true,  true)  => (number & 0x80) == 0x80
        }
    }

    def applyRule() = {
        val lst = ((initial.last :: initial) :+ initial.head).sliding(3).map(x => rule(x)).toList
        new ElementaryCellularAutomata(lst, number)
    }

    override def toString() = {
        initial.foldLeft("")((acc:String, x:Boolean) => if (x) acc ++ "+" else acc ++ "-")
    }

}

def printAutomata(automata:ElementaryCellularAutomata, times:Int):Unit = {
    println(automata)
    if (times > 1)
        printAutomata(automata.applyRule(), times - 1)
}

// vim: set ts=4 sw=4 et:
