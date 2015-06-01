package org.oxygen.morecommands.compiler

import org.oxygen.morecommands.compiler

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._

class PScanner extends StdLexical with compiler.PTokens
{
	override def token: Parser[Token] =
		( floatLiteral									^^ FloatLit
		| stringLiteral									^^ StringLit
		| integerLiteral								^^ IntLit
		| '$' ~> identifierEntity						^^ (name => Identifier(s"$$$name"))
		| identifierEntity ~ '.' ~ identifierEntity		^^ (name => Identifier(s"${name._1._1}.${name._2}"))
		| identifierEntity								^^ identifierName
		| EofCh											^^^ EOF
		| delim
		| failure("Illegal character"))

	def stringOf(p: => Parser[Char]): Parser[String] = rep1(p) ^^ (_ mkString "")

	def octalDigit		: Parser[Long] = elem("octal digit", c => '0' <= c && c <= '7') ^^ (_ - '0')
	def binaryDigit		: Parser[Long] = elem("binary digit", c => c == '0' || c == '1') ^^ (_ - '0')
	def hexadecimalDigit: Parser[Long] = accept("hexadecimal digit",
	{
		case c @ ('a' | 'b' | 'c' | 'd' | 'e' | 'f')							=> c - 'a' + 10
		case c @ ('A' | 'B' | 'C' | 'D' | 'E' | 'F')							=> c - 'A' + 10
		case c @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')	=> c - '0'
	})

	def escape: Parser[Char] = '\\' ~>
		( octalDigit ~ octalDigit ~ octalDigit ^^ { case a ~ b ~ c => (a * 64 + b * 8 + c).toChar }
		| (elem('x') | 'X') ~> hexadecimalDigit ~ hexadecimalDigit ^^ { case a ~ b => (a * 16 + b).toChar }
		| '\\'
		| '\"'
		| '\''
		| 'b' ^^^ '\b'
		| 't' ^^^ '\t'
		| 'n' ^^^ '\n'
		| 'r' ^^^ '\r')

	def floatLiteral: Parser[Double] =
		stringOf(digit) ~ '.' ~ stringOf(digit)		^^ { case int ~ '.' ~ fract => s"$int.$fract".toFloat }

	def stringLiteral: Parser[String] =
		( '\'' ~> stringOf(chrExcept('\\', '\'', EofCh) | escape) <~ '\''
		| '\"' ~> stringOf(chrExcept('\\', '\"', EofCh) | escape) <~ '\"')

	def integerLiteral: Parser[Long] =
		( ('0' ~ (elem('b') | 'B')) ~> rep1(binaryDigit)		^^ { digits => digits.foldLeft(0L)(_ *  2 + _) }
		| ('0' ~ (elem('x') | 'X')) ~> rep1(hexadecimalDigit)	^^ { digits => digits.foldLeft(0L)(_ * 16 + _) }
		| '0' ~> rep1(octalDigit)								^^ { digits => digits.foldLeft(0L)(_ *  8 + _) }
		| stringOf(digit)										^^ ( digits => digits.toLong ))

	def identifierName(ident: String) = ident match
	{
		case "true"		=> BooleanLit(value = true)
		case "false"	=> BooleanLit(value = false)
		case _			=> FunctionName(ident)
	}

	def identifierEntity: Parser[String] =
		identChar ~ rep(identChar | digit) ^^ { case first ~ rest => first :: rest mkString "" }
}
