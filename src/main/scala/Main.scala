import value.{Value, ValueRepository}

object Main extends App {
    
    implicit val repository: ValueRepository = new ValueRepository {}
    val value1 = Value(true)
    val value2 = Value(false)
    println(value1 && value2 get)
    
}
