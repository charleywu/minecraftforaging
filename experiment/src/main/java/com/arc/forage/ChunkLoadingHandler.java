package com.arc.forage;

import java.util.List;

import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeChunkManager.LoadingCallback;
import net.minecraftforge.common.ForgeChunkManager.Ticket;

public class ChunkLoadingHandler implements LoadingCallback
{
	@Override
	public void ticketsLoaded(List<Ticket> tickets, World world) 
	{
	}
}
