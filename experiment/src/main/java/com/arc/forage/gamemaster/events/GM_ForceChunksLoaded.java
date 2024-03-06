package com.arc.forage.gamemaster.events;

import java.util.List;

import com.arc.forage.gamemaster.ChunkLoader;
import com.arc.forage.gamemaster.ForagePlayer;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.structures.Field;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.chunk.Chunk;

public class GM_ForceChunksLoaded implements GMEvent {

	List<ForagePlayer> players;
	
	public GM_ForceChunksLoaded(List<ForagePlayer> players) {
		this.players = players;
	}

	@Override
	public boolean process(int tsle) {
		for(ForagePlayer p : players) {
			BlockPos player_pos = p.get_player().getPosition();
			
			// Force chunk the player is in
			ChunkPos cp = GameMaster.server.getEntityWorld().getChunkFromBlockCoords(player_pos).getPos();
			ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), cp);
			
			// Force neighboring chunks if the player is near them
			int tol = 5;
			if(player_pos.getX()-tol < cp.getXStart()) ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), new ChunkPos(cp.x-1, cp.z));
			if(player_pos.getZ()-tol < cp.getZStart()) ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), new ChunkPos(cp.x, cp.z-1));
			if(player_pos.getX()-tol < cp.getXStart() && player_pos.getZ()-tol < cp.getZStart()) ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), new ChunkPos(cp.x-1, cp.z-1));
			
			if(player_pos.getX()+tol > cp.getXEnd()) ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), new ChunkPos(cp.x+1, cp.z));
			if(player_pos.getZ()+tol > cp.getZEnd()) ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), new ChunkPos(cp.x, cp.z+1));
			if(player_pos.getX()+tol > cp.getXEnd() && player_pos.getZ()+tol < cp.getZEnd()) ChunkLoader.force_chunk(GameMaster.server.getEntityWorld(), new ChunkPos(cp.x+1, cp.z+1));			
		}
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.FORCECHUNKSLOADED;
	}

	@Override
	public String get_status() {
		return "Forcing loaded chunks...";
	}

}
