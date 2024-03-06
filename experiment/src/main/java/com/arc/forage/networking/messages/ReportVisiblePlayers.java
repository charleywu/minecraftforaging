package com.arc.forage.networking.messages;

import java.nio.charset.Charset;

import io.netty.buffer.ByteBuf;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraftforge.fml.common.network.ByteBufUtils;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

public class ReportVisiblePlayers implements IMessage {

	public String visible_Players;
	public String sender;
	
	public ReportVisiblePlayers() {	} //Default constructor is requried
	
	public ReportVisiblePlayers(String sender, String v) {
		this.sender = sender;
		visible_Players = v;
	}
	
	@Override
	public void fromBytes(ByteBuf buf) {
		sender = ByteBufUtils.readUTF8String(buf);
		visible_Players = ByteBufUtils.readUTF8String(buf);
	}

	@Override
	public void toBytes(ByteBuf buf) {
		ByteBufUtils.writeUTF8String(buf, sender);
		ByteBufUtils.writeUTF8String(buf, visible_Players);
	}

}
