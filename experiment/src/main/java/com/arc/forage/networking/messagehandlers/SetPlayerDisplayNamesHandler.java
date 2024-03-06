package com.arc.forage.networking.messagehandlers;

import java.util.UUID;

import com.arc.forage.Forage;
import com.arc.forage.events.HideTags;
import com.arc.forage.events.RenderPlayer;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.PlayerVisTracker;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.messages.ReportVisiblePlayers;
import com.arc.forage.networking.messages.SetPlayerDisplayNames;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

public class SetPlayerDisplayNamesHandler implements IMessageHandler<SetPlayerDisplayNames, IMessage> {

	@Override
	public IMessage onMessage(SetPlayerDisplayNames message, MessageContext ctx) {
		RenderPlayer.clearDisplayNames();
		
		String[] playernames = SetPlayerDisplayNames.deserializeArray(message.player_names);
		String[] playerdisplaynames = SetPlayerDisplayNames.deserializeArray(message.player_displaynames);
		
		for(int i=0; i<Math.min(playernames.length, playerdisplaynames.length); i++) {
			System.out.println("Setting: " + playernames[i] + " TO " + playerdisplaynames[i]);
			RenderPlayer.setDisplayName(playernames[i], playerdisplaynames[i]);
			
			// If any name is empty, we'll simply hide all nametags. Otherwise not.
			HideTags.hideTags = playerdisplaynames[i].equals("");
			
			// There is a very small chance that this runs JUST when a player connects/disconnects, when "world" is null. This try-catch is to prevent the game from crashing. 
			try {
				Minecraft.getMinecraft().world.getPlayerEntityByName(playernames[i]).refreshDisplayName();
			} catch(Exception e) {
				
			}
		}
		
		System.out.println("HIDETAGS STATUS: " + HideTags.hideTags);
		
		return null;
	}

}

