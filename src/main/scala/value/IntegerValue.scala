package value

import value.DecimalValue.{DecimalAdd, DecimalDivide, DecimalMultiply, DecimalSubtract}
import value.IntegerValue.{IntegerAdd, IntegerDivide, IntegerMod, IntegerMultiply, IntegerNegate, IntegerSubtract}

import scala.language.implicitConversions

abstract class IntegerValue extends ComparableValue {
    override protected type T = Long
    
    final def unary_+ : IntegerValue = this
    
    final def unary_- : IntegerValue = IntegerNegate(this)
    
    // add
    final def +(that: IntegerValue): IntegerValue = IntegerAdd(this, that)
    
    final def +(that: DecimalValue): DecimalValue = DecimalAdd(this.toDecimalValue, that)
    
    // subtract
    final def -(that: IntegerValue): IntegerValue = IntegerSubtract(this, that)
    
    final def -(that: DecimalValue): DecimalValue = DecimalSubtract(this.toDecimalValue, that)
    
    // multiply
    final def *(that: IntegerValue): IntegerValue = IntegerMultiply(this, that)
    
    final def *(that: DecimalValue): DecimalValue = DecimalMultiply(this.toDecimalValue, that)
    
    // divide
    final def /(that: IntegerValue): IntegerValue = IntegerDivide(this, that)
    
    final def /(that: DecimalValue): DecimalValue = DecimalDivide(this.toDecimalValue, that)
    
    // modulo
    final def %(that: IntegerValue): IntegerValue = IntegerMod(this, that)
}

object IntegerValue {
    
    implicit def toIntegerValue(value: Byte): IntegerValue = {
        IntegerConstant(value)
    }
    
    implicit def toIntegerValue(value: Short): IntegerValue = {
        IntegerConstant(value)
    }
    
    implicit def toIntegerValue(value: Int): IntegerValue = {
        IntegerConstant(value)
    }
    
    implicit def toIntegerValue(value: Long): IntegerValue = {
        IntegerConstant(value)
    }
    
    final case class IntegerConstant(value: Long) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            Some(value)
        }
    }
    
    final case class IntegerNegate(value: IntegerValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            value.get match {
                case Some(v) => Some(-v)
                case _ => None
            }
        }
    }
    
    final case class IntegerAdd(value1: IntegerValue, value2: IntegerValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class IntegerSubtract(value1: IntegerValue, value2: IntegerValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 - v2)
                case _ => None
            }
        }
    }
    
    final case class IntegerMultiply(value1: IntegerValue, value2: IntegerValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 * v2)
                case _ => None
            }
        }
    }
    
    final case class IntegerDivide(value1: IntegerValue, value2: IntegerValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 / v2)
                case _ => None
            }
        }
    }
    
    final case class IntegerMod(value1: IntegerValue, value2: IntegerValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 % v2)
                case _ => None
            }
        }
    }
    
    final case class ToInteger(value: Value) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            value.get match {
                case Some(v: Long) => Some(v)
                case Some(v: Double) => Some(v.toLong)
                case Some(v: Boolean) => Some(if (v) 1 else 0)
                case Some(v: String) => try {
                    Some(v.toLong)
                } catch {
                    case _: NumberFormatException => None
                }
                case _ => None
            }
        }
    }
    
}
