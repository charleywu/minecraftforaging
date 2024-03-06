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

public class get extends CommandBase {

	@Override
	public String getName() {
		return "get";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "Prints respective ForageSetting entry";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		if(args.length>0) GameMaster.broadcast(args[0] + ": " + ForageSettings.get_string(args[0]));
	}

}
