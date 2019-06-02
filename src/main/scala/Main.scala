import value.BooleanValue.ConstantBoolean
import value.{BooleanValue, ValueRepository}

object Main extends App {
    
    implicit val repository: ValueRepository = new ValueRepository {}
    val value1: BooleanValue = ConstantBoolean(true)
    println(value1.get)
    
}
