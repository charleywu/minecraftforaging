package com.arc.forage.gamemaster;

import java.util.ArrayList;
import java.util.List;

import com.arc.forage.Forage;

import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeChunkManager;
import net.minecraftforge.common.ForgeChunkManager.Ticket;

public class ChunkLoader {
	private static Ticket ticket;
	
	public static void init(World world) {
		try {
			if(ticket==null) ticket = ForgeChunkManager.requestTicket(Forage.INSTANCE, world, ForgeChunkManager.Type.NORMAL);
			System.out.println(">> ChunkLoader max-depth:" + ticket.getMaxChunkListDepth());
		}
		catch(Exception e) {
			System.out.println(">> ERR: Failed to request ticket.");
		}
	}
	
	public static void force_chunk(World world, ChunkPos pos) {
		try {
			if(ticket==null) System.out.println(">> ERR: Chunkloader has no valid ticket!");
			else {
				System.out.println("Forcing chunk: " + pos.x + " : " + pos.z);
				ForgeChunkManager.forceChunk(ticket, pos);
			}
		}
		catch(Exception e) {
			System.out.println(">> ERR: Failed to force chunk.");
		}
	}
	
	public static void unforce_all_chunks(World world) {
		try {
			if(ticket==null) System.out.println(">> ERR: Chunkloader has no valid ticket!");
			ForgeChunkManager.releaseTicket(ticket);
			ticket = ForgeChunkManager.requestTicket(Forage.INSTANCE, world, ForgeChunkManager.Type.NORMAL);
		}
		catch(Exception e) {
			System.out.println(">> ERR: Failed to unload chunk.");
		}
	}
}
