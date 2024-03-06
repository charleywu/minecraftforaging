package com.arc.forage.events;

import com.arc.forage.blocks.RewardBlock;
import com.arc.forage.init.ModBlocks;

import net.minecraftforge.event.world.BlockEvent.BreakEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

@EventBusSubscriber
public class KeepBlocks {
	
	static boolean enabled=true;
	
	public static void toggle() {
		enabled = !enabled;
	}
	
	public static void set_active(boolean v) {
		enabled = v;
	}
	
	@SubscribeEvent
	public static void onBreakBlock(BreakEvent event)
	{
		//Dont allow player to break any blocks except melons and pumpkins
		if(enabled && !(event.getState().getBlock() instanceof RewardBlock)) {
			event.setCanceled(true);
		}
	}

}
