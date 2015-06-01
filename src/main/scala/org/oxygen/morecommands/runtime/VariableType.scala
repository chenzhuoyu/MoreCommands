package org.oxygen.morecommands.runtime

object VariableType
{
	val Any		: Int = -1

	/* Primitive types */
	val Int		: Int = 0
	val Float	: Int = 1
	val String	: Int = 2
	val Boolean	: Int = 3

	/* Compond types */
	val Map		: Int = 4
	val Array	: Int = 5

	def typeToName(vtype: Int): String = vtype match
	{
		case Int		=> "integer"
		case Float		=> "float"
		case String		=> "string"
		case Boolean	=> "boolean"

		case Map		=> "map"
		case Array		=> "array"

		case _			=> "<Unknown>"
	}
}
