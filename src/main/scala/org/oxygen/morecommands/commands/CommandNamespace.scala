package org.oxygen.morecommands.commands

import java.util

import net.minecraft.command.{CommandBase, CommandException, ICommandSender, WrongUsageException}
import net.minecraft.util.BlockPos
import org.oxygen.morecommands.runtime.{Storage, Variable}

import scala.collection.mutable

class CommandNamespace extends CommandBase
{
	override def getName: String = "namespace"
	override def getCommandUsage(sender: ICommandSender): String = "commands.namespace.usage"
	override def getRequiredPermissionLevel: Int = 2

	override def execute(sender: ICommandSender, args: Array[String]): Unit =
	{
		if (args.length < 2)
			throw new WrongUsageException("commands.namespace.usage")

		val command: String = args(0)
		val namespace: String = args(1)

		command match
		{
			case "del" =>
				if (!Storage.namespaces.contains(namespace))
					throw new CommandException("commands.namespace.notfound", namespace)

				Storage.namespaces.remove(namespace)
				CommandBase.notifyOperators(sender, this, "commands.namespace.confirm.del", namespace)

			case "new" =>
				if (Storage.namespaces.contains(namespace))
					throw new CommandException("commands.namespace.exists", namespace)

				Storage.namespaces.put(namespace, new mutable.HashMap[String, Variable]())
				CommandBase.notifyOperators(sender, this, "commands.namespace.confirm.new", namespace)

			case "clear" =>
				if (!Storage.namespaces.contains(namespace))
					throw new CommandException("commands.namespace.notfound", namespace)

				Storage.namespaces(namespace).clear()
				CommandBase.notifyOperators(sender, this, "commands.namespace.confirm.clear", namespace)

			case _ => throw new WrongUsageException("commands.namespace.usage")
		}
	}

	override def addTabCompletionOptions(sender: ICommandSender, args: Array[String], pos: BlockPos): util.List[_] =
	{
		args.length match
		{
			case 1 => CommandBase.getListOfStringsMatchingLastWord(args, "del", "new", "clear")
			case 2 => CommandBase.getListOfStringsMatchingLastWord(args, Storage.namespaces.keys.toArray:_*)
			case _ => null
		}
	}
}
