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

public class GM_UnloadChunks implements GMEvent {
	
	public GM_UnloadChunks() {
	}

	@Override
	public boolean process(int tsle) {
		ChunkLoader.unforce_all_chunks(GameMaster.server.getEntityWorld());
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.UNLOADCHUNKS;
	}

	@Override
	public String get_status() {
		return "Unloading all chunks...";
	}

}
