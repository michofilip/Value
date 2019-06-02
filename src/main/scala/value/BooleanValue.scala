package value

import value.BooleanValue.{AND, NOT, OR}

import scala.language.implicitConversions

abstract class BooleanValue extends Value {
    override protected type T = Boolean
    
    final def unary_! : BooleanValue = NOT(this)
    
    final def &&(that: BooleanValue): BooleanValue = AND(this, that)
    
    final def ||(that: BooleanValue): BooleanValue = OR(this, that)
}

object BooleanValue {
    
    implicit def toBooleanValue(value: Boolean): BooleanValue = {
        ConstantBoolean(value)
    }
    
    final case class ConstantBoolean(value: Boolean) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            Some(value)
        }
    }
    
    final case class NOT(value: BooleanValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            value.get match {
                case Some(v) => Some(!v)
                case _ => None
            }
        }
    }
    
    final case class AND(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 && v2)
                case _ => None
            }
        }
    }
    
    final case class OR(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 || v2)
                case _ => None
            }
        }
    }
    
    final case class Equals(value1: Value, value2: Value) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 == v2)
                case _ => None
            }
        }
    }
    
    final case class Unequals(value1: Value, value2: Value) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1), Some(v2)) => Some(v1 != v2)
                case _ => None
            }
        }
    }
    
    final case class Less(value1: ComparableValue, value2: ComparableValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 < v2)
                
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 < v2)
                
                case _ => None
            }
        }
    }
    
    final case class LessEqual(value1: ComparableValue, value2: ComparableValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 <= v2)
                
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 <= v2)
                
                case _ => None
            }
        }
    }
    
    final case class Greater(value1: ComparableValue, value2: ComparableValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 > v2)
                
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 > v2)
                
                case _ => None
            }
        }
    }
    
    final case class GreaterEqual(value1: ComparableValue, value2: ComparableValue) extends BooleanValue {
        override def get(implicit valueRepository: ValueRepository): Option[Boolean] = {
            (value1.get, value2.get) match {
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 >= v2)
                
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 >= v2)
                
                case _ => None
            }
        }
    }
    
}
