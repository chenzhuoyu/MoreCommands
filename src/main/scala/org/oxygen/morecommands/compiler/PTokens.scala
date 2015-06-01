package org.oxygen.morecommands.compiler

import scala.util.parsing.combinator.token

trait PTokens extends token.StdTokens
{
	case class IntLit(value: Long) extends Token
	{
		override def chars = value.toString
		override def toString = chars
	}

	case class FloatLit(value: Double) extends Token
	{
		override def chars = value.toString
		override def toString = chars
	}

	case class BooleanLit(value: Boolean) extends Token
	{
		override def chars = value.toString
		override def toString = chars
	}

	case class FunctionName(value: String) extends Token
	{
		override def chars = value.toString
		override def toString = chars
	}
}
