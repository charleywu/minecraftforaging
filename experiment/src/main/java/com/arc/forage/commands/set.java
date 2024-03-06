package com.arc.forage.commands;

import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.structures.Field;
import com.arc.forage.tools.FieldTable;

import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;

public class set extends CommandBase {

	@Override
	public String getName() {
		return "set";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "Sets respective ForageSetting entry";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		if(args.length>1) ForageSettings.add(args[0], args[1]);
		GameMaster.broadcast("Set " + args[0] + " to " + args[1]);
	}

}
