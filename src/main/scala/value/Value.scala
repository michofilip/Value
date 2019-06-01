package value

import value.BooleanValue.{Equals, Unequals}
import value.DecimalValue.ToDecimal
import value.IntegerValue.ToInteger

abstract class Value {
    protected type T
    
    def get(implicit valueRepository: ValueRepository): Option[T]
    
    def getOrElse(default: => T)(implicit valueRepository: ValueRepository): T = {
        get match {
            case Some(value) => value
            case None => default
        }
    }
    
    def ===(that: Value): BooleanValue = {
        Equals(this, that)
    }
    
    def !==(that: Value): BooleanValue = {
        Unequals(this, that)
    }
    
    def toIntegerValue: IntegerValue = {
        ToInteger(this)
    }
    
    def toDecimalValue: DecimalValue = {
        ToDecimal(this)
    }
}
