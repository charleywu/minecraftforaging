package com.arc.forage.gamemaster;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import com.arc.forage.structures.Field;

import net.minecraft.client.renderer.culling.Frustum;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

public class DataLogger {
	
	static FileWriter player_events_log;
	static FileWriter block_events_log;
	static String dir = "datalogs/";
	
	public static void init(String gamename, int round_index) {
		try {			
			player_events_log = new FileWriter(dir+"/"+gamename+"_"+round_index+"_players.log.csv");
			block_events_log = new FileWriter(dir+"/"+gamename+"_"+round_index+"_blocks.log.csv");
			
			String pel_columns = "\"Time since round start\";\"Player Name\";\"X-Position\";\"Z-Position\";\"X-Look\";\"Y-Look\";\"Z-Look\";\"Visible Players\";\n";
			String bel_columns = "\"Time since round start\";\"Player Name\";\"X-Position\";\"Z-Position\";\"Reward\";\n";
			
			player_events_log.append(pel_columns);
			block_events_log.append(bel_columns);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void init_folder() {
		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-d_HH-mm-ss");  
		LocalDateTime now = LocalDateTime.now();  
		
		dir = "datalogs/"+dtf.format(now);
		new File(dir).mkdir();
	}
	
	public static void uninit() {
		try {
			if(player_events_log!=null) player_events_log.close();
			if(block_events_log!=null) block_events_log.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void log_block(EntityPlayer player, BlockPos pos, boolean reward) {
		Field f = GameMaster.getFP(player).get_field();
		float time = GameMaster.time_since_round_start_f();
		try {
			double relative_x = pos.getX() - f.get_origin().getX();
			double relative_z = pos.getZ() - f.get_origin().getZ();
			block_events_log.append(time+";"+player.getName()+";"+relative_x+";"+relative_z+";"+reward+";\n");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void log_players(List<ForagePlayer> players) {
		for(ForagePlayer p : players) log_player(p, players);
	}
	
	public static void log_player(ForagePlayer p, List<ForagePlayer> players) {
		float time = GameMaster.time_since_round_start_f();
		String vis_players = PlayerVisTracker.get_vis(p.get_player().getName());
		try {
			double relative_x = p.get_player().posX - p.get_field().get_origin().getX();
			double relative_z = p.get_player().posZ - p.get_field().get_origin().getZ();
			Vec3d lookd = p.get_player().getLookVec();
			player_events_log.append(time+";"+p.get_player().getName()+";"+relative_x+";"+relative_z+";"+lookd.x+";"+lookd.y+";"+lookd.z+";"+vis_players+";\n");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
