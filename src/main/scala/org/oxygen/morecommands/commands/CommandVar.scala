package org.oxygen.morecommands.commands

import java.util

import net.minecraft.command.{CommandBase, CommandException, ICommandSender, WrongUsageException}
import net.minecraft.util.BlockPos
import org.oxygen.morecommands.runtime.{Storage, Variable}

import scala.collection.mutable

class CommandVar extends CommandBase
{
	override def getName: String = "var"
	override def getCommandUsage(sender: ICommandSender): String = "commands.var.usage"
	override def getRequiredPermissionLevel: Int = 2

	override def execute(sender: ICommandSender, args: Array[String]): Unit =
	{
		if (args.length < 3)
			throw new WrongUsageException("commands.var.usage")

		val name: String = args(2)
		val command: String = args(0)
		val namespace: String = args(1)

		if (!Storage.namespaces.contains(namespace))
			throw new CommandException("commands.var.namespace", namespace)

		val storage: mutable.HashMap[String, Variable] = Storage.namespaces(namespace)

		command match
		{
			case "del" => storage.remove(name) match
			{
				case None		=> throw new CommandException("commands.var.variable", s"$namespace.$name")
				case Some(_)	=> CommandBase.notifyOperators(sender, this, "commands.var.confirm.del", s"$namespace.$name")
			}

			case "def" => args.length match
			{
				case 4 => args(3) match
				{
					case "float"	=> storage(name) = new Variable(0.0)
					case "string"	=> storage(name) = new Variable("")
					case "integer"	=> storage(name) = new Variable(0)
					case "boolean"	=> storage(name) = new Variable(false)
					case "map"		=> storage(name) = new Variable(new mutable.HashMap[Variable, Variable])
					case "array"	=> storage(name) = new Variable(new mutable.ArrayBuffer[Variable])
					case _			=> throw new WrongUsageException("commands.var.usage")
				}

				CommandBase.notifyOperators(sender, this, "commands.var.confirm.def", s"$namespace.$name", args(3))

				case 5 => try
				{
					args(3) match
					{
						case "float"	=> storage(name) = new Variable(args(4).toFloat)
						case "integer"	=> storage(name) = new Variable(args(4).toInt)
						case "boolean"	=> storage(name) = new Variable(args(4).toBoolean)
						case "array"	=> storage(name) = new Variable(mutable.ArrayBuffer.fill(args(4).toInt)(new Variable(0)))
						case _			=> throw new WrongUsageException("commands.var.init", args(3))
					}

					CommandBase.notifyOperators(sender, this, "commands.var.confirm.def", s"$namespace.$name", args(3))
				} catch
				{
					case e: NumberFormatException		=> throw new CommandException("commands.var.number", args(4))
					case e: IllegalArgumentException	=> throw new CommandException("commands.var.boolean", args(4))
				}

				case _ => throw new WrongUsageException("commands.var.usage")
			}

			case "dump" => storage.get(name) match
			{
				case None			=> throw new CommandException("commands.var.variable", s"$namespace.$name")
				case Some(value)	=> CommandBase.notifyOperators(sender, this, "commands.var.confirm.dump", s"$namespace.$name", value)
			}

			case _ => throw new WrongUsageException("commands.var.usage")
		}

		sender.sendCommandFeedback()
	}

	override def addTabCompletionOptions(sender: ICommandSender, args: Array[String], pos: BlockPos): util.List[_] =
	{
		args.length match
		{
			case 1 => CommandBase.getListOfStringsMatchingLastWord(args, "def", "del", "dump")
			case 2 => CommandBase.getListOfStringsMatchingLastWord(args, Storage.namespaces.keys.toArray:_*)
			case 3 => Storage.namespaces.get(args(1)) match
			{
				case Some(storage)	=> CommandBase.getListOfStringsMatchingLastWord(args, storage.keys.toArray:_*)
				case None			=> null
			}

			case 4 => args(0) match
			{
				case "def"	=> CommandBase.getListOfStringsMatchingLastWord(args, "float", "string", "integer", "boolean", "map", "array")
				case _		=> null
			}

			case _ => null
		}
	}
}
