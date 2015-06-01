package org.oxygen.morecommands.compiler

import org.objectweb.asm.{Label, ClassWriter, MethodVisitor, Opcodes}

import scala.collection.immutable
import scala.language.postfixOps

object PAssembler
{
	final val Int			: Int = 0
	final val Float			: Int = 1
	final val String		: Int = 2
	final val Boolean		: Int = 3

	final val OP_SET		: Int = 0
	final val OP_ADD		: Int = 1
	final val OP_SUB		: Int = 2
	final val OP_MUL		: Int = 3
	final val OP_DIV		: Int = 4
	final val OP_MOD		: Int = 5
	final val OP_AND		: Int = 6
	final val OP_OR			: Int = 7
	final val OP_XOR		: Int = 8
	final val OP_SHL		: Int = 9
	final val OP_SHR		: Int = 10
	final val OP_USHR		: Int = 11

	final val Sender		: String = "net/minecraft/command/ICommandSender"
	final val Storage		: String = "org/oxygen/morecommands/runtime/Storage"
	final val Variable		: String = "org/oxygen/morecommands/runtime/Variable"

	final val UNARIES		= immutable.HashMap[String, String](
		"$~"	-> "unary_$tilde",
		"$!"	-> "unary_$bang",
		"$-"	-> "unary_$minus")

	final val BINARIES		= immutable.HashMap[String, String](
		"+"		-> "$plus",
		"-"		-> "$minus",
		"*"		-> "$times",
		"/"		-> "$div",
		"%"		-> "$percent",
		"&"		-> "$amp",
		"|"		-> "$bar",
		"^"		-> "$up",
		"**"	-> "$times$times",
		"<<"	-> "$less$less",
		">>"	-> "$greater$greater",
		">>>"	-> "$greater$greater$greater",

		">"		-> "$greater",
		"<"		-> "$less",
		"=="	-> "$eq$eq",
		"!="	-> "$bang$eq",
		">="	-> "$greater$eq",
		"<="	-> "$less$eq")

	final val OPERATORS		= immutable.HashMap[String, Int](
		"="		-> OP_SET,
		"+="	-> OP_ADD,
		"-="	-> OP_SUB,
		"*="	-> OP_MUL,
		"/="	-> OP_DIV,
		"%="	-> OP_MOD,
		"&="	-> OP_AND,
		"|="	-> OP_OR,
		"^="	-> OP_XOR,
		"<<="	-> OP_SHL,
		">>="	-> OP_SHR,
		">>>="	-> OP_USHR
	)

	class Expr(val left: Expr, val op: String, val right: Expr)
	{
		def +(v: Expr): Expr	= new Expr(this, "+", v)
		def -(v: Expr): Expr	= new Expr(this, "-", v)
		def *(v: Expr): Expr	= new Expr(this, "*", v)
		def /(v: Expr): Expr	= new Expr(this, "/", v)
		def %(v: Expr): Expr	= new Expr(this, "%", v)
		def &(v: Expr): Expr	= new Expr(this, "&", v)
		def |(v: Expr): Expr	= new Expr(this, "|", v)
		def ^(v: Expr): Expr	= new Expr(this, "^", v)
		def **(v: Expr): Expr	= new Expr(this, "**", v)
		def &&(v: Expr): Expr	= new Expr(this, "&&", v)
		def ||(v: Expr): Expr	= new Expr(this, "||", v)
		def <<(v: Expr): Expr	= new Expr(this, "<<", v)
		def >>(v: Expr): Expr	= new Expr(this, ">>", v)
		def >>>(v: Expr): Expr	= new Expr(this, ">>>", v)

		def >(v: Expr): Expr	= new Expr(this, ">", v)
		def <(v: Expr): Expr	= new Expr(this, "<", v)
		def ==(v: Expr): Expr	= new Expr(this, "==", v)
		def !=(v: Expr): Expr	= new Expr(this, "!=", v)
		def >=(v: Expr): Expr	= new Expr(this, ">=", v)
		def <=(v: Expr): Expr	= new Expr(this, "<=", v)

		def unary_~ : Expr		= new Expr(null, "$~", this)
		def unary_! : Expr		= new Expr(null, "$!", this)
		def unary_- : Expr		= new Expr(null, "$-", this)

		def assemble(method: MethodVisitor): Unit = op match
		{
			case "&&" | "||" =>
				val label = new Label
				left.assemble(method)
				method.visitInsn(Opcodes.DUP)
				method.visitMethodInsn(Opcodes.INVOKEVIRTUAL, Variable, "isTrue", "()Z", false)
				method.visitJumpInsn(if (op == "&&") Opcodes.IFEQ else Opcodes.IFNE, label)
				method.visitInsn(Opcodes.POP)
				right.assemble(method)
				method.visitLabel(label)

			case _ => left.eq(null) match
			{
				case true	=>
					right.assemble(method)
					applyOperator(method, op)

				case false	=>
					left.assemble(method)
					right.assemble(method)
					applyOperator(method, op)
			}
		}

		override def toString: String = s"{Expr $left $op $right}"
	}

	class Name(val name: String) extends Expr(null, "$=>", null)
	{
		def <++ : Expr = new Save(name, "+=", new Value(1))
		def <-- : Expr = new Save(name, "-=", new Value(1))
		def >++ : Expr = new Expr(new Save(name, "+=", new Value(1)), "-", new Value(1))
		def >-- : Expr = new Expr(new Save(name, "-=", new Value(1)), "+", new Value(1))

		override def toString: String = s"{Name $name}"
		override def assemble(method: MethodVisitor): Unit =
		{
			method.visitLdcInsn(name)
			method.visitMethodInsn(Opcodes.INVOKESTATIC, PAssembler.Storage, "resolveName", s"(Ljava/lang/String;)L$Variable;", false)
		}
	}

	class Call(val name: String, val args: List[Expr]) extends Expr(null, "$()", null)
	{
		override def toString: String = args.isEmpty match
		{
			case true	=> s"{Call $name}"
			case false	=> s"{Call $name [${args.map(_.toString).reduceLeft(_ + ", " + _)}}]}"
		}

		override def assemble(method: MethodVisitor): Unit =
		{
			method.visitLdcInsn(name)
			method.visitVarInsn(Opcodes.ALOAD, 0)
			method.visitLdcInsn(args.length)
			method.visitTypeInsn(Opcodes.ANEWARRAY, PAssembler.Variable)

			for ((expr, i) <- args zipWithIndex)
			{
				method.visitInsn(Opcodes.DUP)
				method.visitLdcInsn(i)
				args(i).assemble(method)
				method.visitInsn(Opcodes.AASTORE)
			}

			method.visitMethodInsn(Opcodes.INVOKESTATIC, Storage, "invokeFunction",
				s"(Ljava/lang/String;L${PAssembler.Sender};[L${PAssembler.Variable};)L${PAssembler.Variable};", false)
		}
	}

	class Save(val name: String, val action: String, val value: Expr) extends Expr(null, "$<=", value)
	{
		override def toString: String = s"{$action $name $value}"
		override def assemble(method: MethodVisitor): Unit =
		{
			method.visitLdcInsn(name)
			method.visitLdcInsn(OPERATORS(action))
			value.assemble(method)
			method.visitMethodInsn(Opcodes.INVOKESTATIC, PAssembler.Storage,
				"replaceName", s"(Ljava/lang/String;IL$Variable;)L$Variable;", false)
		}
	}

	class Value extends Expr(null, "$=", null)
	{
		var vtype		: Int = 0
		var intValue	: Long = 0
		var floatValue	: Double = 0.0
		var stringValue	: String = ""
		var booleanValue: Boolean = false

		def this(value: Long) =
		{
			this()
			vtype = Int
			intValue = value
		}

		def this(value: Double) =
		{
			this()
			vtype = Float
			floatValue = value
		}

		def this(value: String) =
		{
			this()
			vtype = String
			stringValue = value
		}

		def this(value: Boolean) =
		{
			this()
			vtype = Boolean
			booleanValue = value
		}

		override def toString: String = vtype match
		{
			case Int		=> s"{=i $intValue}"
			case Float		=> s"{=f $floatValue}"
			case String		=> s"{=s $stringValue}"
			case Boolean	=> s"{=b $booleanValue}"
		}

		override def assemble(method: MethodVisitor): Unit =
		{
			method.visitTypeInsn(Opcodes.NEW, PAssembler.Variable)
			method.visitInsn(Opcodes.DUP)

			vtype match
			{
				case Int		=>
					method.visitLdcInsn(intValue)
					method.visitMethodInsn(Opcodes.INVOKESPECIAL, PAssembler.Variable, "<init>", "(J)V", false)

				case Float		=>
					method.visitLdcInsn(floatValue)
					method.visitMethodInsn(Opcodes.INVOKESPECIAL, PAssembler.Variable, "<init>", "(D)V", false)

				case String		=>
					method.visitLdcInsn(stringValue)
					method.visitMethodInsn(Opcodes.INVOKESPECIAL, PAssembler.Variable, "<init>", "(Ljava/lang/String;)V", false)

				case Boolean	=>
					method.visitLdcInsn(booleanValue)
					method.visitMethodInsn(Opcodes.INVOKESPECIAL, PAssembler.Variable, "<init>", "(Z)V", false)
			}
		}
	}

	def applyOperator(method: MethodVisitor, operator: String): Unit = UNARIES.get(operator) match
	{
		case Some(func)	=> method.visitMethodInsn(Opcodes.INVOKEVIRTUAL, Variable, func, s"()L$Variable;", false)
		case None		=> BINARIES.get(operator) match
		{
			case None		=> throw new RuntimeException(s"Invalid operator '$operator'")
			case Some(func)	=> method.visitMethodInsn(Opcodes.INVOKEVIRTUAL, Variable, func, s"(L$Variable;)L$Variable;", false)
		}
	}

	def assembleClass(name: String, assemble: => (MethodVisitor) => Unit): Class[_] =
	{
		val writter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
		writter.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name.replace('.', '/'), null, "java/lang/Object", null)

		val method = writter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "eval", s"(L$Sender;)L$Variable;", null, null)
		method.visitCode()
		assemble(method)
		method.visitMaxs(0, 0)
		method.visitEnd()
		writter.visitEnd()

		val code = writter.toByteArray
		val loader = getClass.getClassLoader
		val injector = classOf[ClassLoader].getDeclaredMethod("defineClass",
			classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])

		injector.setAccessible(true)
		injector.invoke(loader, name, code, 0: Integer, code.length: Integer).asInstanceOf[Class[_]]
	}
}
