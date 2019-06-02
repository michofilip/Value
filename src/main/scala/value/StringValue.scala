package value

import value.StringValue.{Concatenate, Length}

import scala.language.implicitConversions

abstract class StringValue extends Value {
    override protected type T = String
    
    final def +(that: StringValue): StringValue = Concatenate(this, that)
    
    final def length: IntegerValue = Length(this)
}

object StringValue {
    
    implicit def toStringValue(value: String): StringValue = {
        StringConstant(value)
    }
    
    final case class StringConstant(value: String) extends StringValue {
        override def get(implicit valueRepository: ValueRepository): Option[String] = {
            Some(value)
        }
    }
    
    final case class Concatenate(value1: StringValue, value2: StringValue) extends StringValue {
        override def get(implicit valueRepository: ValueRepository): Option[String] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class Length(value: StringValue) extends IntegerValue {
        override def get(implicit valueRepository: ValueRepository): Option[Long] = {
            value.get match {
                case Some(v) => Some(v.length)
                case _ => None
            }
        }
    }
    
    final case class NumericToString(value: Value) extends StringValue {
        override def get(implicit valueRepository: ValueRepository): Option[String] = {
            value.get match {
                case Some(v: Long) => Some(v.toString)
                case Some(v: Double) => Some(v.toString)
                case Some(v: Boolean) => Some(v.toString)
                case Some(v: String) => Some(v)
                case _ => None
            }
        }
    }
    
}
