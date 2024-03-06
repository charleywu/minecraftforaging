package com.arc.forage.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.arc.forage.Forage;
import com.arc.forage.gamemaster.ForagePlayer;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.Network;
import com.arc.forage.networking.messages.ReportVisiblePlayers;
import com.arc.forage.networking.messages.SetBlockHardness;
import com.arc.forage.settings.ForageSettings;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraftforge.client.event.RenderPlayerEvent;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.Side;

@Mod.EventBusSubscriber(value = Side.CLIENT, modid = Forage.MODID)
public class RenderPlayer {
	
	static List<String> visiblePlayers_lastframe = new ArrayList<String>();
	static List<String> visiblePlayers_thisframe = new ArrayList<String>();
	static Map<String, String> playerDisplayNames = new HashMap<String, String>();
	
	public static void clearDisplayNames() {
		playerDisplayNames.clear();
	}
	
	public static void setDisplayName(String playername, String displayname) {
		playerDisplayNames.put(playername, displayname);
	}
	
	// Set player nametag
	@SubscribeEvent
	public static void renderName(PlayerEvent.NameFormat event) {
		String name = event.getEntityPlayer().getName();
		if(playerDisplayNames.containsKey(name)) {
			event.setDisplayname(playerDisplayNames.get(name));
		}
	}
	
	// Set visible players list
	@SubscribeEvent
	public static void onRenderPlayer(RenderPlayerEvent.Post event) {
		String pname = event.getEntityPlayer().getName();
		if(!visiblePlayers_thisframe.contains(pname)) visiblePlayers_thisframe.add(pname);
	}
	
	// Set visible player list to server
	//If performance is impacted, replace this with PlayerTickEvent or only execute every few frames
	@SubscribeEvent
	public static void onLastRender(RenderWorldLastEvent event) {
		Collections.sort(visiblePlayers_thisframe);
		if(!visiblePlayers_thisframe.equals(visiblePlayers_lastframe)) {
			String send = String.join(",", visiblePlayers_thisframe);
			send = "[" + send + "]";
			Network.INSTANCE.sendToServer(new ReportVisiblePlayers(Minecraft.getMinecraft().player.getName(), send));
		}
		visiblePlayers_lastframe = visiblePlayers_thisframe;
		visiblePlayers_thisframe = new ArrayList<String>();
	}
}
