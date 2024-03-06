package com.arc.forage.networking.messages;

import io.netty.buffer.ByteBuf;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

public class SetBlockHardness implements IMessage {

	public float new_hardness;
	public int block_id;
	
	public SetBlockHardness() {	} //Default constructor is requried
	
	public SetBlockHardness(Block block, float f) {
		this.new_hardness = f;
		this.block_id = Block.getIdFromBlock(block);
	}
	
	@Override
	public void fromBytes(ByteBuf buf) {
		block_id = buf.readInt();
		new_hardness = buf.readFloat();
	}

	@Override
	public void toBytes(ByteBuf buf) {
		buf.writeInt(block_id);
		buf.writeFloat(new_hardness);
	}

}
