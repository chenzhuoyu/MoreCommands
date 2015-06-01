package org.oxygen.morecommands

import java.io._
import java.util

import net.minecraft.command._
import net.minecraft.command.server.CommandTestForBlock
import net.minecraft.entity.Entity
import net.minecraft.nbt.{JsonToNBT, NBTTagCompound}
import net.minecraft.server.MinecraftServer
import net.minecraft.world.WorldServer
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.event.world.WorldEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.Mod.EventHandler
import net.minecraftforge.fml.common.event.{FMLServerStartingEvent, FMLServerStoppedEvent}
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import org.oxygen.morecommands.commands.{CommandEval, CommandNamespace, CommandVar}
import org.oxygen.morecommands.runtime.{Storage, Variable, VariableType}

import scala.collection.JavaConversions._
import scala.collection.mutable

@Mod(modid = "MoreCommands", name = "More Commands", version = "0.1a", modLanguage = "scala")
object MoreCommands
{
	private def saveVariables(): Unit =
	{
		println(s"Saving variables to world '${MinecraftServer.getServer.getFolderName}' ...")

		try
		{
			val stream: ObjectOutputStream = getOutputStream

			stream.writeObject(Storage.environ)
			stream.writeObject(Storage.namespaces)
		} catch
		{
			case e: FileNotFoundException =>
				println("*** WARNING: Failed to save variables !!! All values are LOST !!!")
		}
	}

	private def getInputStream: ObjectInputStream =
	{
		val worldName: String = MinecraftServer.getServer.getFolderName

		try
		{
			new ObjectInputStream(new FileInputStream(s"$worldName/variables.dat"))
		} catch
		{
			case e: FileNotFoundException =>
				new ObjectInputStream(new FileInputStream(s"saves/$worldName/variables.dat"))
		}
	}

	private def getOutputStream: ObjectOutputStream =
	{
		val worldName: String = MinecraftServer.getServer.getFolderName

		try
		{
			new ObjectOutputStream(new FileOutputStream(s"$worldName/variables.dat"))
		} catch
		{
			case e: FileNotFoundException =>
				new ObjectOutputStream(new FileOutputStream(s"saves/$worldName/variables.dat"))
		}
	}

	@EventHandler def serverLoad(event: FMLServerStartingEvent): Unit =
	{
		Storage.environ.clear()
		Storage.functions.clear()
		Storage.namespaces.clear()
		println(s"Loading variables from world '${MinecraftServer.getServer.getFolderName}' ...")

		try
		{
			val stream: ObjectInputStream = getInputStream

			Storage.environ ++= readObjectFromStream[Storage.environ.type](stream)
			Storage.namespaces ++= readObjectFromStream[Storage.namespaces.type](stream)
		} catch
		{
			case e: FileNotFoundException =>
				println("variables.dat not exists, ignored")
		}

		println("Registering event listeners ....")
		MinecraftForge.EVENT_BUS.register(this)

		println("Registering server commands ....")
		event.registerServerCommand(new CommandVar)
		event.registerServerCommand(new CommandEval)
		event.registerServerCommand(new CommandNamespace)

		println("Registering built-in type methods ....")
		Storage.registerTypeMethods()

		println("Registering built-in functions ....")
		Storage.functions("exec") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			Storage.checkArgsExact("exec", args, VariableType.String)

			val command: String = args.head.stringValue
			val manager: ICommandManager = MinecraftServer.getServer.getCommandManager

			if (manager.executeCommand(sender, command) < 1)
				throw new CommandException("commands.execute.allInvocationsFailed", command)

			new Variable(true)
		}

		Storage.functions("execif") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			Storage.checkArgsExact("execif", args, VariableType.Boolean, VariableType.String)

			val command: String = args(1).stringValue
			val manager: ICommandManager = MinecraftServer.getServer.getCommandManager

			if (args.head.booleanValue && manager.executeCommand(sender, command) < 1)
				throw new CommandException("commands.execute.allInvocationsFailed", command)

			args.head
		}

		Storage.functions("coords") = (sender: ICommandSender, args: Array[Variable]) =>
		{
			var tag: NBTTagCompound = null

			if (Storage.checkArgsRange("coords", args, 1, VariableType.String, VariableType.String) == 2)
				tag = JsonToNBT.func_180713_a(args(1).stringValue)

			val result = new mutable.ArrayBuffer[Variable]
			val entities = PlayerSelector.matchEntities(sender, args.head.stringValue, classOf[Entity])

			for (entity <- entities.asInstanceOf[util.List[Entity]])
			{
				var matchesNBT = true

				if (tag != null)
				{
					val entityTag = new NBTTagCompound

					entity.writeToNBT(entityTag)
					matchesNBT = CommandTestForBlock.func_175775_a(tag, entityTag, true)
				}

				if (matchesNBT)
				{
					val coord = new mutable.HashMap[Variable, Variable]

					coord(new Variable("x")) = new Variable(entity.posX)
					coord(new Variable("y")) = new Variable(entity.posY)
					coord(new Variable("z")) = new Variable(entity.posZ)
					result += new Variable(coord)
				}
			}

			sender.setCommandStat(CommandResultStats.Type.SUCCESS_COUNT, 1)
			new Variable(result)
		}
	}

	@EventHandler def serverUnload(event: FMLServerStoppedEvent): Unit =
	{
		println("Unegistering event listeners ....")
		MinecraftForge.EVENT_BUS.unregister(this)
		saveVariables()
	}

	@SubscribeEvent def onWorldSaved(event: WorldEvent.Save): Unit =
	{
		if (event.world.getClass == classOf[WorldServer])
			saveVariables()
	}

	private def readObjectFromStream[A](stream: ObjectInputStream)(implicit m:scala.reflect.Manifest[A]): A =
	{
		stream.readObject() match
		{
			case v if m.runtimeClass.isInstance(v) => v.asInstanceOf[A]
			case _ => throw new RuntimeException("Type not what was expected when reading from file")
		}
	}
}
