package com.arc.forage.commands;

import com.arc.forage.events.KeepBlocks;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.structures.Field;
import com.arc.forage.tools.FieldTable;

import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;

public class blockbreak extends CommandBase {

	@Override
	public String getName() {
		return "blockbreak";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "starts game";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		KeepBlocks.toggle();
	}

}
