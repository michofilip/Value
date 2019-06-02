import value.BooleanValue.BooleanConstant
import value.{BooleanValue, ValueRepository}

object Main extends App {
    
    implicit val repository: ValueRepository = new ValueRepository {}
    val value1: BooleanValue = BooleanConstant(true)
    println(value1.get)
    
}
