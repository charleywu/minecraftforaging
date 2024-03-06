package com.arc.forage.commands;

import com.arc.forage.blocks.RewardBlock;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.PlayerScoreTracker;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.structures.Field;
import com.arc.forage.structures.NBTStructure;
import com.arc.forage.tools.FieldTable;

import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;

public class getscores extends CommandBase {

	@Override
	public String getName() {
		return "getscores";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "yields current player scores";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		PlayerScoreTracker.print_all_scores();
	}

}