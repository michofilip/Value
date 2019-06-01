package value

import value.DecimalValue.{DecimalAdd, DecimalDivide, DecimalMultiply, DecimalNegate, DecimalSubtract}


abstract class DecimalValue extends ComparableValue {
    override protected type T = Double
    
    final def unary_+ : DecimalValue = this
    
    final def unary_- : DecimalValue = DecimalNegate(this)
    
    // add
    final def +(that: IntegerValue): DecimalValue = DecimalAdd(this, that.toDecimalValue)
    
    final def +(that: DecimalValue): DecimalValue = DecimalAdd(this, that)
    
    // subtract
    final def -(that: IntegerValue): DecimalValue = DecimalSubtract(this, that.toDecimalValue)
    
    final def -(that: DecimalValue): DecimalValue = DecimalSubtract(this, that)
    
    // multiply
    final def *(that: IntegerValue): DecimalValue = DecimalMultiply(this, that.toDecimalValue)
    
    final def *(that: DecimalValue): DecimalValue = DecimalMultiply(this, that)
    
    // divide
    final def /(that: IntegerValue): DecimalValue = DecimalDivide(this, that.toDecimalValue)
    
    final def /(that: DecimalValue): DecimalValue = DecimalDivide(this, that)
}

object DecimalValue {
    
    final case class DecimalConstant(value: Int) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            Some(value)
        }
    }
    
    final case class DecimalNegate(value: DecimalValue) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            value.get match {
                case Some(v) => Some(-v)
                case _ => None
            }
        }
    }
    
    final case class DecimalAdd(value1: DecimalValue, value2: DecimalValue) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class DecimalSubtract(value1: DecimalValue, value2: DecimalValue) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 - v2)
                case _ => None
            }
        }
    }
    
    final case class DecimalMultiply(value1: DecimalValue, value2: DecimalValue) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 * v2)
                case _ => None
            }
        }
    }
    
    final case class DecimalDivide(value1: DecimalValue, value2: DecimalValue) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 / v2)
                case _ => None
            }
        }
    }
    
    final case class ToDecimal(value: Value) extends DecimalValue {
        override def get(implicit valueRepository: ValueRepository): Option[Double] = {
            value.get match {
                case Some(v: Long) => Some(v.toDouble)
                case Some(v: Double) => Some(v)
                case Some(v: Boolean) => Some(if (v) 1 else 0)
                case Some(v: String) => try {
                    Some(v.toDouble)
                } catch {
                    case _: NumberFormatException => None
                }
                case _ => None
            }
        }
    }
    
}