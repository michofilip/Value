object Main extends App {
    
    abstract class Value() {
        type T
        
        def get: Option[T]
    }
    
    case class IntValue(v: Int) extends Value {
        override type T = Int
        
        override def get: Option[Int] = Some(v)
    }
    
    case object NullValue extends Value {
        override type T = Unit
        
        override def get: Option[Unit] = None
    }
    
    class NegValue private(value: Either[IntValue, NullValue.type]) extends Value {
        override type T = Int
        
        def this(value: IntValue) {
            this(Left(value))
        }
        
        def this(value: NullValue.type) {
            this(Right(value))
        }
        
        override def get: Option[Int] = value match {
            case Left(intV) => intV.get match {
                case Some(v: Int) => Some(-v)
                case _ => None
            }
            case Right(_) => None
            
        }
    }
    
    val v1 = new IntValue(10)
    val v2 = new NegValue(v1)
    val v3 = new NegValue(NullValue)
    
    println(v1.get)
    println(v2.get)
    println(v3.get)
    
    
}
