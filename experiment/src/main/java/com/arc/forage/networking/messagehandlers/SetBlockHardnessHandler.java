package com.arc.forage.networking.messagehandlers;

import com.arc.forage.Forage;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.messages.SetBlockHardness;

import net.minecraft.block.Block;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.common.network.simpleimpl.SimpleNetworkWrapper;

public class SetBlockHardnessHandler implements IMessageHandler<SetBlockHardness, IMessage> {

	@Override
	public IMessage onMessage(SetBlockHardness message, MessageContext ctx) {
		Block.getBlockById(message.block_id).setHardness(message.new_hardness);
		return null;
	}

}

