package com.arc.forage.structures;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;

public class Structure {
	protected World world;
	protected BlockPos origin;
	protected boolean exists;
	
	protected Vec3i size;
	
	public boolean exists() {
		return exists;
	}
	
	public Vec3i get_size() {
		return size;
	}
	
	public BlockPos get_center() {
		return origin;
	}
	
	public BlockPos get_origin() {
		return origin;
	}
	
	public World get_world() {
		return world;
	}
	
	public Structure(World world, BlockPos origin, Vec3i size) {
		this.world = world;
		this.origin = origin;
		this.size = size;
	}
	
	public Structure(World world, JSONStructure js) {
		this.world = world;
		this.origin = new BlockPos(js.pos_x, js.pos_y, js.pos_z);
		this.size = new Vec3i(js.siz_x, js.siz_y, js.siz_z);
	}
	
	public void clear() {
		System.out.println("Clearing structure at " + origin + " Size: " + size);
		
		//This is faster and leaner but only works when the respective chunks are loaded.
		//MinecraftServer s = world.getMinecraftServer();
		//s.getCommandManager().executeCommand(s, "/fill "+origin.getX()+" "+origin.getY()+" "+origin.getZ()+" "+(origin.getX()+size.getX())+" "+(origin.getY()+size.getY())+" "+(origin.getZ()+size.getZ())+" air");
		
		for(int x=origin.getX(); x<origin.getX()+size.getX(); x++) {
			for(int y=origin.getY(); y<origin.getY()+size.getY(); y++) {
				for(int z=origin.getZ(); z<origin.getZ()+size.getZ(); z++) {
					world.setBlockToAir(new BlockPos(x, y, z));
				}
			}
		}		
		exists = false;
	}
	
	public void place() {
		
	}
	
	public JSONStructure toJSON() {
		return new JSONStructure(origin.getX(), origin.getY(), origin.getZ(), size.getX(), size.getY(), size.getZ());
	}
}