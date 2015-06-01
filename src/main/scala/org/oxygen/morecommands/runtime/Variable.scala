package org.oxygen.morecommands.runtime

import scala.collection.mutable

@SerialVersionUID(1)
class Variable extends Serializable
{
	var vtype		: Int = 0

	/* Primitive types */
	var intValue	: Long = 0
	var floatValue	: Double = 0.0
	var stringValue	: String = ""
	var booleanValue: Boolean = false

	/* Compound types */
	var mapValue	= new mutable.HashMap[Variable, Variable]
	var arrayValue	= new mutable.ArrayBuffer[Variable]

	def this(value: Long) =
	{
		this()
		mapValue.clear()
		arrayValue.clear()

		vtype = VariableType.Int
		intValue = value
	}

	def this(value: Double) =
	{
		this()
		mapValue.clear()
		arrayValue.clear()

		vtype = VariableType.Float
		floatValue = value
	}

	def this(value: String) =
	{
		this()
		mapValue.clear()
		arrayValue.clear()

		vtype = VariableType.String
		stringValue = value
	}

	def this(value: Boolean) =
	{
		this()
		mapValue.clear()
		arrayValue.clear()

		vtype = VariableType.Boolean
		booleanValue = value
	}

	def this(value: mutable.Map[Variable, Variable]) =
	{
		this()
		mapValue.clear()
		arrayValue.clear()

		vtype = VariableType.Map
		mapValue ++= value
	}

	def this(value: mutable.ArrayBuffer[Variable]) =
	{
		this()
		mapValue.clear()
		arrayValue.clear()

		vtype = VariableType.Array
		arrayValue ++= value
	}

	def +(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue + v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue + v.floatValue)
		case (VariableType.Int		, VariableType.String	) => new Variable(intValue + v.stringValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue + v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue + v.floatValue)
		case (VariableType.Float	, VariableType.String	) => new Variable(floatValue + v.stringValue)
		case (VariableType.String	, VariableType.Int		) => new Variable(stringValue + v.intValue)
		case (VariableType.String	, VariableType.Float	) => new Variable(stringValue + v.floatValue)
		case (VariableType.String	, VariableType.String	) => new Variable(stringValue + v.stringValue)
		case (VariableType.String	, VariableType.Boolean	) => new Variable(stringValue + v.booleanValue)
		case (VariableType.Boolean	, VariableType.String	) => new Variable(booleanValue + v.stringValue)
		case (VariableType.Map		, VariableType.Map		) => new Variable(mapValue ++ v.mapValue)

		case (_						, VariableType.String	) => new Variable(toString + v.stringValue)
		case (VariableType.String	, _						) => new Variable(stringValue + v.toString)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '+'")
	}

	def -(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue - v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue - v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue - v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue - v.floatValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '-'")
	}

	def *(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue * v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue * v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue * v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue * v.floatValue)
		case (VariableType.String	, VariableType.Int		) => new Variable(stringValue * v.intValue.toInt)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '*'")
	}

	def /(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue / v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue / v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue / v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue / v.floatValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '/'")
	}

	def %(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue % v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue % v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue % v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue % v.floatValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '%'")
	}

	def &(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int, VariableType.Int) => new Variable(intValue & v.intValue)
		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '&'")
	}

	def |(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int, VariableType.Int) => new Variable(intValue | v.intValue)
		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '|'")
	}

	def ^(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int, VariableType.Int) => new Variable(intValue ^ v.intValue)
		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '^'")
	}

	def **(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(math.round(math.pow(intValue, v.intValue)))
		case (VariableType.Int		, VariableType.Float	) => new Variable(math.pow(intValue, v.floatValue))
		case (VariableType.Float	, VariableType.Int		) => new Variable(math.pow(floatValue, v.intValue))
		case (VariableType.Float	, VariableType.Float	) => new Variable(math.pow(floatValue, v.floatValue))

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '**'")
	}

	def <<(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int, VariableType.Int) => new Variable(intValue << v.intValue)
		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '<<'")
	}

	def >>(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int, VariableType.Int) => new Variable(intValue >> v.intValue)
		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '>>'")
	}

	def >>>(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int, VariableType.Int) => new Variable(intValue >>> v.intValue)
		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '>>>'")
	}

	def unary_~ : Variable = vtype match
	{
		case VariableType.Int => new Variable(~intValue)
		case _ => throw new ArithmeticException(s"Undefined behavior with unary '~' and type '${VariableType.typeToName(vtype)}'")
	}

	def unary_! : Variable = vtype match
	{
		case VariableType.Boolean => new Variable(!booleanValue)
		case _ => throw new ArithmeticException(s"Undefined behavior with unary '!' and type '${VariableType.typeToName(vtype)}'")
	}

	def unary_- : Variable = vtype match
	{
		case VariableType.Int	 	=> new Variable(-intValue)
		case VariableType.Float	 	=> new Variable(-floatValue)

		case _ => throw new ArithmeticException(s"Undefined behavior with unary '-' and type '${VariableType.typeToName(vtype)}'")
	}

	def >(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue > v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue > v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue > v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue > v.floatValue)
		case (VariableType.String	, VariableType.String	) => new Variable(stringValue > v.stringValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '>'")
	}

	def <(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue < v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue < v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue < v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue < v.floatValue)
		case (VariableType.String	, VariableType.String	) => new Variable(stringValue < v.stringValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '<'")
	}

	def ==(v: Variable): Variable = new Variable(equals(v))
	def !=(v: Variable): Variable = new Variable(!equals(v))

	def >=(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue >= v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue >= v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue >= v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue >= v.floatValue)
		case (VariableType.String	, VariableType.String	) => new Variable(stringValue >= v.stringValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '>='")
	}

	def <=(v: Variable): Variable = (vtype, v.vtype) match
	{
		case (VariableType.Int		, VariableType.Int		) => new Variable(intValue <= v.intValue)
		case (VariableType.Int		, VariableType.Float	) => new Variable(intValue <= v.floatValue)
		case (VariableType.Float	, VariableType.Int		) => new Variable(floatValue <= v.intValue)
		case (VariableType.Float	, VariableType.Float	) => new Variable(floatValue <= v.floatValue)
		case (VariableType.String	, VariableType.String	) => new Variable(stringValue <= v.stringValue)

		case _ => throw new ArithmeticException("Undefined behavior between type " +
			s"'${VariableType.typeToName(vtype)}' and '${VariableType.typeToName(v.vtype)}' with operator '<='")
	}

	def isTrue: Boolean = vtype match
	{
		case VariableType.Int		=> intValue != 0
		case VariableType.Float		=> floatValue != 0.0
		case VariableType.String	=> !stringValue.isEmpty
		case VariableType.Boolean	=> booleanValue

		case VariableType.Map		=> !mapValue.isEmpty
		case VariableType.Array		=> !arrayValue.isEmpty
	}

	override def equals(o: Any): Boolean = o match
	{
		case v: Variable	=> (vtype, v.vtype) match
		{
			case (VariableType.Int		, VariableType.Int		) => intValue == v.intValue
			case (VariableType.Float	, VariableType.Float	) => floatValue == v.floatValue
			case (VariableType.String	, VariableType.String	) => stringValue == v.stringValue
			case (VariableType.Boolean	, VariableType.Boolean	) => booleanValue == v.booleanValue

			case (VariableType.Map		, VariableType.Map		) => mapValue == v.mapValue
			case (VariableType.Array	, VariableType.Array	) => arrayValue == v.arrayValue
			case _												  => false
		}

		case _				=> false
	}

	override def hashCode: Int = vtype match
	{
		case VariableType.Int		=> intValue.hashCode
		case VariableType.Float		=> floatValue.hashCode
		case VariableType.String	=> stringValue.hashCode
		case VariableType.Boolean	=> booleanValue.hashCode

		case VariableType.Map		=> mapValue.hashCode()
		case VariableType.Array		=> arrayValue.hashCode()

		case _ => throw new RuntimeException(s"Unknown variable type $vtype")
	}

	override def toString: String = vtype match
	{
		case VariableType.Int		=> s"$intValue"
		case VariableType.Float		=> f"$floatValue%.2f"
		case VariableType.String	=> stringValue
		case VariableType.Boolean	=> s"$booleanValue"

		case VariableType.Map		=> mapValue.isEmpty match
		{
			case true	=> "{}"
			case false	=> s"{${mapValue.flatMap(x => List(x._1.toString + ": " + x._2.toString)).reduce(_ + ", " + _)}}"
		}

		case VariableType.Array		=> arrayValue.isEmpty match
		{
			case true	=> "[]"
			case false	=> s"[${arrayValue.map(_.toString).reduce(_ + ", " + _)}]"
		}

		case _ => throw new RuntimeException(s"Unknown variable type $vtype")
	}
}
