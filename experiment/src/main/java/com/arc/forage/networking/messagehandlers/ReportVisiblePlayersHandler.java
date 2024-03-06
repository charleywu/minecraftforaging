package com.arc.forage.networking.messagehandlers;

import com.arc.forage.Forage;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.PlayerVisTracker;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.messages.ReportVisiblePlayers;

import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

public class ReportVisiblePlayersHandler implements IMessageHandler<ReportVisiblePlayers, IMessage> {

	@Override
	public IMessage onMessage(ReportVisiblePlayers message, MessageContext ctx) {
		PlayerVisTracker.set_vis(message.sender, message.visible_Players);
		return null;
	}

}

