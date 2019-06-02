package value

import value.BooleanValue.{BooleanConstant, Equals, Unequals}
import value.DecimalValue.{DecimalConstant, ToDecimal}
import value.IntegerValue.{IntegerConstant, ToInteger}
import value.StringValue.StringConstant

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

object Value {
    def apply(value: Boolean): BooleanValue = BooleanConstant(value)
    
    def apply(value: Byte): IntegerValue = IntegerConstant(value)
    
    def apply(value: Short): IntegerValue = IntegerConstant(value)
    
    def apply(value: Int): IntegerValue = IntegerConstant(value)
    
    def apply(value: Long): IntegerValue = IntegerConstant(value)
    
    def apply(value: Float): DecimalValue = DecimalConstant(value)
    
    def apply(value: Double): DecimalValue = DecimalConstant(value)
    
    def apply(value: String): StringValue = StringConstant(value)
}
