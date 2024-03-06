package com.arc.forage.structures;

import com.arc.forage.blocks.RewardBlock;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.tools.FieldTable;

import net.minecraft.block.Block;
import net.minecraft.block.BlockColored;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.item.EnumDyeColor;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;

public class Field extends Structure {
	private FieldTable table;
	private int interval;
	private int padding;
	private Block rb;
	
	public Field(World world, BlockPos origin, FieldTable table, int interval, Block rb) {
		super(world, origin, new Vec3i(table.row_count*interval, 4, table.clm_count*interval));
		this.table = table;
		this.interval = Math.max(interval, 1); // Minimum of interval=2, otherwise the redstone underneath fail
		this.padding = interval-1; //additional blocks placed on origin-edges
		this.rb = rb;
		size = new Vec3i(size.getX()+padding, size.getY(), size.getZ()+padding);
	}
	
	@Override
	public BlockPos get_center() {
		return new BlockPos(origin).add(new Vec3i(size.getX()/2, 2, size.getZ()/2));
	}
	
	public BlockPos get_random() {
		//The +1 and -2 numbers are to prevent players from spawning beyond the fence.
		int block_x = (int) (Math.random()*(size.getX()-2)+1);
		int block_z = (int) (Math.random()*(size.getZ()-2)+1);
		
		// Prevent people from spawning inside a pumpkin/melon
		if((block_x-padding) % interval == 0 && (block_z-padding) % interval == 0) block_x++;
		
		return new BlockPos(origin).add(new Vec3i(block_x, 2, block_z));
	}
	
	@Override
	public void place() {
		if(!table.initialized || exists) { System.out.println(">> ERR: Field already exists or table is not initialized!"); return; }
		
		int x = origin.getX();
		int y = origin.getY();
		int z = origin.getZ();
		
		for(int x_offset=0; x_offset<size.getX(); x_offset++) {
			for(int z_offset=0; z_offset<size.getZ(); z_offset++) {
				// Place brown wool:
				setBlock(x+x_offset, y+1, z+z_offset, world, Blocks.WOOL.getDefaultState().withProperty(BlockColored.COLOR, EnumDyeColor.BROWN));
				// Place fences around edge:
				if(x_offset==0 || x_offset==size.getX()-1 || z_offset==0 || z_offset==size.getZ()-1) setBlock(x+x_offset, y+2, z+z_offset, world, Blocks.OAK_FENCE.getDefaultState());
				// Place reward blocks
				if((x_offset-padding) % interval == 0 && (z_offset-padding) % interval == 0) {
					boolean incl_reward = table.get(x_offset/interval, z_offset/interval);
					
					// Place block itself
					setBlock(x+x_offset, y+2, z+z_offset, world, rb.getDefaultState());
					// Place torches when in dev mode
					if(ForageSettings.get_bool("/game/dev") && incl_reward) setBlock(x+x_offset, y+3, z+z_offset, world, Blocks.TORCH.getDefaultState());
					// Blue underblock
					if(ForageSettings.get_bool("/game/settings/underblock") && incl_reward) setBlock(x+x_offset, y+1, z+z_offset, world, Blocks.WOOL.getDefaultState().withProperty(BlockColored.COLOR, EnumDyeColor.BLUE));
					// Contingency mode NOTE: Turn off if interval <2
					if(incl_reward) setBlock(x+x_offset, y, z+z_offset, world, Blocks.REDSTONE_BLOCK.getDefaultState());
				}
			}
		}
		exists = true;
	}
	
	private static void setBlock(int x, int y, int z, World world, IBlockState block) {
		world.setBlockState(new BlockPos(x, y, z), block, 2);
	}
}
