package com.arc.forage.gamemaster;

import java.util.ArrayList;

import com.arc.forage.structures.Field;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;

public class ForagePlayer {
	
	static ArrayList<Integer> player_ids = new ArrayList<Integer>();
	
	private EntityPlayer player;
	private int id;
	
	public boolean check_ready() {
		BlockPos underPlayer = player.getPosition().add(0, -1, 0);
		if(player.onGround && player.world.getBlockState(underPlayer)==Blocks.EMERALD_BLOCK.getDefaultState()) {
			return true;
		}
		return false;
	}

	public int get_id() {
		return id;
	}
	
	public EntityPlayer get_player() {
		return player;
	}
	
	public Field get_field() {
		if(GameMaster.fields.size()>1) return GameMaster.fields.get(id);
		return GameMaster.fields.get(0);
	}
	
	public ForagePlayer(EntityPlayer p) {
		player = p;
		id = getNewId();
		player_ids.add(id);
	}
	
	public void Destroy() {
		player_ids.remove(Integer.valueOf(id));
	}
	
	private int getNewId() {
		int i=0;
		while(player_ids.contains(Integer.valueOf(i))) i++;
		return i;
	}
	
}
