package com.arc.forage.commands;

import com.arc.forage.blocks.RewardBlock;
import com.arc.forage.gamemaster.ChunkLoader;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.events.GM_ForceChunksLoaded;
import com.arc.forage.gamemaster.events.GM_UnloadChunks;
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
import net.minecraft.world.World;

public class status extends CommandBase {

	@Override
	public String getName() {
		return "status";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "yields current game status";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		GameMaster.broadcast_status();
		if(args[0].equals("force")) {
			GameMaster.push_event(new GM_ForceChunksLoaded(GameMaster.players));
		}
		else {
			GameMaster.push_event(new GM_UnloadChunks());
		}
	}

}
