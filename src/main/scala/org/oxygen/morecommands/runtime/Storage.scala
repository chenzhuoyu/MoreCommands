package org.oxygen.morecommands.runtime

import net.minecraft.command.{ICommandSender, CommandException}
import org.oxygen.morecommands.compiler.PAssembler

import scala.collection.mutable
import scala.language.postfixOps

object Storage
{
	val environ = new mutable.HashMap[String, Variable]
	val functions = new mutable.HashMap[String, (ICommandSender, Array[Variable]) => Variable]
	val namespaces = new mutable.HashMap[String, mutable.HashMap[String, Variable]]

	def findFunction(name: String): (ICommandSender, Array[Variable]) => Variable = functions.get(name) match
	{
		case Some(func)	=> func
		case None		=> throw new CommandException("commands.eval.function", name)
	}

	def findNamespace(name: String) =
	{
		var key = name
		var namespace = Storage.environ

		if (!name.startsWith("$"))
		{
			val names: Array[String] = name.split('.')

			if (names.length != 2)
				throw new CommandException("commands.eval.invalid_ident", name)

			if (!Storage.namespaces.contains(names(0)))
				throw new CommandException("commands.eval.namespace", names(0))

			key = names(1)
			namespace = Storage.namespaces(names(0))
		}

		if (!namespace.contains(key))
			throw new CommandException("commands.eval.variable", key)

		(key, namespace)
	}

	def resolveName(name: String): Variable =
	{
		val (key, namespace) = findNamespace(name)
		namespace(key)
	}

	def replaceName(name: String, op: Int, value: Variable): Variable =
	{
		if (op == PAssembler.OP_SET && name.startsWith("$"))
		{
			environ(name) = value
			return value
		}

		val (key, namespace) = findNamespace(name)

		op match
		{
			case PAssembler.OP_ADD	=> namespace(key) += value
			case PAssembler.OP_SUB	=> namespace(key) -= value
			case PAssembler.OP_MUL	=> namespace(key) *= value
			case PAssembler.OP_DIV	=> namespace(key) /= value
			case PAssembler.OP_MOD	=> namespace(key) %= value
			case PAssembler.OP_AND	=> namespace(key) &= value
			case PAssembler.OP_OR	=> namespace(key) |= value
			case PAssembler.OP_XOR	=> namespace(key) ^= value
			case PAssembler.OP_SHL	=> namespace(key) <<= value
			case PAssembler.OP_SHR	=> namespace(key) >>= value
			case PAssembler.OP_USHR	=> namespace(key) >>>= value

			/* Assignments need to verify type compability */
			case PAssembler.OP_SET	=>
				if (value.vtype == namespace(key).vtype)
					namespace(key) = value
				else
					throw new CommandException("commands.eval.type",
						VariableType.typeToName(value.vtype), VariableType.typeToName(namespace(key).vtype))
		}

		namespace(key)
	}

	def invokeFunction(name: String, sender: ICommandSender, args: Array[Variable]): Variable = functions.get(name) match
	{
		case Some(func)	=> func(sender, args)
		case None		=> throw new CommandException("commands.eval.function", name)
	}

	def checkArgsExact(name: String, args: Array[Variable], types: Int*): Unit =
	{
		if (args.length != types.length)
			throw new CommandException("commands.function.arguments.count.exact", name, types.length: Integer)

		for (((arg, atype), i) <- args zip types zipWithIndex)
			if (arg.vtype != atype && atype != VariableType.Any)
				throw new CommandException("commands.function.arguments.type.only", name, VariableType.typeToName(atype), i + 1: Integer)
	}

	def checkArgsRange(name: String, args: Array[Variable], least: Int, types: Int*): Int =
	{
		if (args.length < least)
			throw new CommandException("commands.function.arguments.count.least", name, types.length: Integer)

		if (args.length > types.length)
			throw new CommandException("commands.function.arguments.count.most", name, types.length: Integer)

		for (((arg, atype), i) <- args zip types zipWithIndex)
			if (arg.vtype != atype && atype != VariableType.Any)
				throw new CommandException("commands.function.arguments.type.only", name, VariableType.typeToName(atype), i + 1: Integer)

		args.length
	}

	def registerTypeMethods(): Unit =
	{
		functions("ceil") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			checkArgsExact("ceil", args, VariableType.Float)
			new Variable(args.head.floatValue.ceil)
		}

		functions("floor") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			checkArgsExact("floor", args, VariableType.Float)
			new Variable(args.head.floatValue.floor)
		}

		functions("round") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			checkArgsExact("round", args, VariableType.Float)
			new Variable(args.head.floatValue.round)
		}

		functions("del") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			if (args.length != 2)
				throw new CommandException("commands.function.arguments.count.exact", "del", 2: Integer)

			args.head.vtype match
			{
				case VariableType.Map	=> args.head.mapValue.remove(args(1)) match
				{
					case Some(value)	=> value
					case None			=> throw new CommandException("commands.map.key", args(1))
				}

				case VariableType.Array	=> args(1).vtype match
				{
					case VariableType.Int => try
					{
						args.head.arrayValue.remove(args(1).intValue.toInt)
					} catch
						{
							case e: IndexOutOfBoundsException =>
								throw new CommandException("commands.array.index", args(1).intValue.toInt: Integer)
						}

					case _ => throw new CommandException("commands.function.arguments.type.only", "del", "integer", 2: Integer)
				}

				case _ => throw new CommandException("commands.function.arguments.type.only", "del", "map or array", 1: Integer)
			}
		}

		functions("get") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			if (args.length != 2)
				throw new CommandException("commands.function.arguments.count.exact", "get", 2: Integer)

			args.head.vtype match
			{
				case VariableType.Map	=> args.head.mapValue.get(args(1)) match
				{
					case Some(value)	=> value
					case None			=> throw new CommandException("commands.map.key", args(1))
				}

				case VariableType.Array	=> args(1).vtype match
				{
					case VariableType.Int => try
					{
						args.head.arrayValue(args(1).intValue.toInt)
					} catch
					{
						case e: IndexOutOfBoundsException =>
							throw new CommandException("commands.array.index", args(1).intValue.toInt: Integer)
					}

					case _ =>
						throw new CommandException("commands.function.arguments.type.only", "get", "integer", 2: Integer)
				}

				case _ =>
					throw new CommandException("commands.function.arguments.type.only", "get", "map or array", 1: Integer)
			}
		}

		functions("set") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			if (args.length != 3)
				throw new CommandException("commands.function.arguments.count.exact", "set", 3: Integer)

			args.head.vtype match
			{
				case VariableType.Map		=> args.head.mapValue.put(args(1), args(2)) match
				{
					case Some(value)		=> args.head
					case None				=> throw new CommandException("commands.map.key", args(1))
				}

				case VariableType.Array		=> args(1).vtype match
				{
					case VariableType.Int	=> try
					{
						args.head.arrayValue(args(1).intValue.toInt) = args(2)
						args.head
					} catch
					{
						case e: IndexOutOfBoundsException =>
							throw new CommandException("commands.array.index", args(1).intValue.toInt: Integer)
					}

					case _ =>
						throw new CommandException("commands.function.arguments.type.only", "set", "integer", 2: Integer)
				}

				case _ =>
					throw new CommandException("commands.function.arguments.type.only", "set", "map or array", 1: Integer)
			}
		}

		functions("len") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			if (args.length != 1)
				throw new CommandException("commands.function.arguments.count.exact", "len", 1: Integer)

			args.head.vtype match
			{
				case VariableType.Map	=> new Variable(args.head.mapValue.size)
				case VariableType.Array	=> new Variable(args.head.arrayValue.length)
				case _					=>
					throw new CommandException("commands.function.arguments.type.only", "len", "map or array", 1: Integer)
			}
		}

		functions("clear") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			if (args.length != 1)
				throw new CommandException("commands.function.arguments.count.exact", "clear", 1: Integer)

			args.head.vtype match
			{
				case VariableType.Map	=>
					args.head.mapValue.clear()
					args.head

				case VariableType.Array	=>
					args.head.arrayValue.clear()
					args.head

				case _ =>
					throw new CommandException("commands.function.arguments.type.only", "clear", "map or array", 1: Integer)
			}
		}

		functions("append") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			checkArgsExact("append", args, VariableType.Array, VariableType.Any)
			args.head.arrayValue += args(1)
			new Variable(args.head.arrayValue)
		}
	}
}
