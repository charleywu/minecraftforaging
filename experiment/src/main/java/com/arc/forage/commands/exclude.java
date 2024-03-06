package com.arc.forage.commands;

import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.structures.Field;
import com.arc.forage.tools.FieldTable;

import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;

public class exclude extends CommandBase {

	@Override
	public String getName() {
		return "exclude";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "Sets user as spectator-admin";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		GameMaster.Exclude(sender.getName());
	}

}
