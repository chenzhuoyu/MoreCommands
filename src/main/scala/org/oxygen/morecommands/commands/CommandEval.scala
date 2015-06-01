package org.oxygen.morecommands.commands

import java.lang.reflect.{InvocationTargetException, Method}

import net.minecraft.command.{CommandException, CommandBase, ICommandSender, WrongUsageException}
import org.objectweb.asm.Opcodes
import org.oxygen.morecommands.compiler.{PAssembler, PCompiler}

import scala.collection.mutable

class CommandEval extends CommandBase
{
	private var classIndex = 1
	private val commandCache = new mutable.HashMap[String, Method]

	override def getName: String = "eval"
	override def getCommandUsage(sender: ICommandSender): String = "commands.eval.usage"
	override def getRequiredPermissionLevel: Int = 2

	private def compile(expr: String): Method =
	{
		val ast = new PCompiler(expr).compile
		val EvalJIT = PAssembler.assembleClass(s"com.oxygen.morecommands.runtime.jit.$$$classIndex", (method) =>
		{
			ast.assemble(method)
			method.visitInsn(Opcodes.ARETURN)
		})

		classIndex += 1
		EvalJIT.getMethod("eval", classOf[ICommandSender])
	}

	override def execute(sender: ICommandSender, args: Array[String]): Unit =
	{
		if (args.length == 0)
			throw new WrongUsageException("commands.eval.usage")

		try
		{
			val expr: String = args.foldLeft("")(_ + " " + _)
			val method: Method = commandCache.getOrElseUpdate(expr, compile(expr))
			CommandBase.notifyOperators(sender, this, "commands.eval.result", method.invoke(null, sender))
		} catch
		{
			case e: RuntimeException =>
				throw new CommandException(s"${e.getClass.getSimpleName}: ${e.getMessage}")

			case e: InvocationTargetException =>
				if (e.getTargetException.isInstanceOf[CommandException])
					throw e.getTargetException /* exception forwarding */
				else
					throw new CommandException(s"${e.getClass.getSimpleName}: ${e.getMessage}")
		}
	}
}
