package org.oxygen.morecommands.compiler

import org.oxygen.morecommands.compiler.PAssembler.{Call, Expr, Name, Save, Value}

import scala.language.postfixOps
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader

class PCompiler(val expr: String) extends StdTokenParsers
{
	type Tokens = PTokens
	override val lexical = new PScanner

	def intLit		: Parser[Long]		= accept("int"		, { case lexical.IntLit(value) => value })
	def floatLit	: Parser[Double]	= accept("float"	, { case lexical.FloatLit(value) => value })
	def booleanLit	: Parser[Boolean]	= accept("boolean"	, { case lexical.BooleanLit(value) => value })
	def functionName: Parser[String]	= accept("function"	, { case lexical.FunctionName(value) => value })

	lexical.delimiters ++= List(
		"("  , ")"  , "[" , "]" ,
		"==" , "!=" , "=" ,
		"&&" , "||" , "!" ,
		"&=" , "|=" , "^=",
		"&"  , "|"  , "^" , "~" ,
		"<<=", ">>=",
		"<<" , ">>" ,
		"+=" , "-=" , "*=", "/=", "%=", "**", "++", "--",
		"+"  , "-"  , "*" , "/" , "%" ,
		"<=" , ">=" ,
		"<"  , ">"
	)

	lazy val subexpr	: Parser[Expr] =
		( assign
		| boolor)

	lazy val call		: Parser[Expr] =
		functionName ~ (subexpr *) ^^ { case name ~ args => new Call(name, args) }

	lazy val factor		: Parser[Expr] =
		( ident <~ "++"			^^ (new Name(_) >++)
		| ident <~ "--"			^^ (new Name(_) >--)
		| ident					^^ (new Name(_))
		| intLit				^^ (new Value(_))
		| floatLit				^^ (new Value(_))
		| stringLit				^^ (new Value(_))
		| booleanLit			^^ (new Value(_))
		| "[" ~> call <~ "]"
		| "(" ~> subexpr <~ ")")

	lazy val unary		: Parser[Expr] =
		( factor
		| "++" ~> ident		^^ (new Name(_) <++)
		| "--" ~> ident		^^ (new Name(_) <--)
		|  "+" ~> factor
		|  "-" ~> factor	^^ (-_)
		|  "~" ~> factor	^^ (~_)
		|  "!" ~> factor	^^ (!_))

	lazy val power		: Parser[Expr] = unary *
		( "**" ^^^ {(x: Expr, y: Expr) => x ** y})

	lazy val term		: Parser[Expr] = power *
		( "*" ^^^ {(x: Expr, y: Expr) => x * y}
		| "/" ^^^ {(x: Expr, y: Expr) => x / y}
		| "%" ^^^ {(x: Expr, y: Expr) => x % y})

	lazy val addsub		: Parser[Expr] = term *
		( "+" ^^^ {(x: Expr, y: Expr) => x + y}
		| "-" ^^^ {(x: Expr, y: Expr) => x - y})

	lazy val shifts		: Parser[Expr] = addsub *
		( "<<" ^^^ {(x: Expr, y: Expr) => x << y}
		| ">>" ^^^ {(x: Expr, y: Expr) => x >> y})

	lazy val compares	: Parser[Expr] = shifts *
		( "<=" ^^^ {(x: Expr, y: Expr) => x <= y}
		| ">=" ^^^ {(x: Expr, y: Expr) => x >= y}
		| "<"  ^^^ {(x: Expr, y: Expr) => x <  y}
		| ">"  ^^^ {(x: Expr, y: Expr) => x >  y})

	lazy val equals		: Parser[Expr] = compares *
		( "!=" ^^^ {(x: Expr, y: Expr) => x != y}
		| "==" ^^^ {(x: Expr, y: Expr) => x == y})

	lazy val bitand		: Parser[Expr] = equals *
		( "&" ^^^ {(x: Expr, y: Expr) => x & y})

	lazy val bitxor		: Parser[Expr] = bitand *
		( "^" ^^^ {(x: Expr, y: Expr) => x ^ y})

	lazy val bitor		: Parser[Expr] = bitxor *
		( "|" ^^^ {(x: Expr, y: Expr) => x | y})

	lazy val booland	: Parser[Expr] = bitor *
		( "&&" ^^^ {(x: Expr, y: Expr) => x && y})

	lazy val boolor		: Parser[Expr] = booland *
		( "||" ^^^ {(x: Expr, y: Expr) => x || y})

	lazy val assign		: Parser[Expr] =
		( ident ~ ( "=" | "+=" | "-=" |  "*=" |  "/=" |   "%=") ~ boolor ^^ { case name ~ op ~ value => new Save(name, op, value) }
		| ident ~ ("&=" | "|=" | "^=" | "<<=" | ">>=" | ">>>=") ~ boolor ^^ { case name ~ op ~ value => new Save(name, op, value) })

	def compile : Expr =
	{
		val statement = expr + CharArrayReader.EofCh
		(subexpr <~ lexical.EOF)(new lexical.Scanner(statement)).get
	}
}
