package com.arc.forage.commands;

import java.io.File;
import java.io.FileReader;

import com.arc.forage.blocks.RewardBlock;
import com.arc.forage.gamemaster.DataLogger;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.L;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.structures.Field;
import com.arc.forage.structures.NBTStructure;
import com.arc.forage.tools.FieldTable;

import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;

public class setup extends CommandBase {
	
	@Override
	public String getName() {
		return "setup";
	}

	@Override
	public String getUsage(ICommandSender sender) {
		return "/setup [json_file]";
	}

	@Override
	public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
		if(args.length<1) {
			System.out.println(">> ERR: No file given!");
			WebAPI.set_error("Could not set up session.");
			return;
		}
			
		WebAPI.set_status("Setting up "+args[0]+".json ...");
		
		// Check if args[0] exists as a session/file
		File f = new File("sessions/"+args[0]+".json");
		if(!f.exists()) {
			System.out.println(">> ERR: File sessions/"+args[0]+".json does not exist. Setup routine interrupted.");
			WebAPI.set_error("Session does not exists!");
			return;
		}
		
		WebAPI.add_info("Ending previous session...");
		GameMaster.force_endGame();
		
		WebAPI.add_info("Reconfiguring...");
		GameMaster.reconfigure(args);
		
		WebAPI.add_info("Initializing DataLogger");
		DataLogger.init_folder();
		
		WebAPI.set_status("Session in progress. Session: " + args[0] + ", Language: " + L.get_loc());
		GameMaster.startRound();
		
		System.out.println("\r\n\r\n>> Session setup completed.");
	}

}
