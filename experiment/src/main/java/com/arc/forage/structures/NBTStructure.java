package com.arc.forage.structures;

import java.util.ArrayList;
import java.util.List;

import com.arc.forage.Forage;
import com.arc.forage.gamemaster.L;
import com.arc.forage.settings.ForageSettings;

import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.Mirror;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.Rotation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;
import net.minecraft.world.gen.structure.template.PlacementSettings;
import net.minecraft.world.gen.structure.template.Template;
import net.minecraft.world.gen.structure.template.TemplateManager;

public class NBTStructure extends Structure {
	protected String nbtFile;
	protected Vec3i center_offset;
	private TemplateManager tmplManager;
	private Template tmpl;
	
	private List<NBTStructure> extensions = new ArrayList<NBTStructure>();
	
	private boolean has_multiple_centers = false;
	private Vec3i m_center_offset;
	private int m_center_amount;

	public NBTStructure(World world, BlockPos origin, Vec3i center_offset, String nbtFile) {
		super(world, origin, new Vec3i(0,0,0)); //We set size to 0 here, which is corrected once we've read the nbt file
		this.nbtFile = nbtFile;
		this.center_offset = center_offset;
		
		WorldServer ws = (WorldServer) world;
		MinecraftServer server = ws.getMinecraftServer();
		
		tmplManager = ws.getStructureTemplateManager();
		tmpl = tmplManager.getTemplate(server, new ResourceLocation(Forage.MODID, nbtFile+"_"+L.get_loc()));
		size = new Vec3i(tmpl.getSize().getX(), tmpl.getSize().getY(), tmpl.getSize().getZ());
	}
	
	public void set_multiple_centers(boolean has_multiple_centers, Vec3i m_center_offset, int m_center_amount) {
		this.has_multiple_centers = has_multiple_centers;
		this.m_center_offset = m_center_offset;
		this.m_center_amount = m_center_amount;
	}
	
	public static NBTStructure fromSettings(World world, String fsprefix, int offset_mul) {
		BlockPos origin = ForageSettings.get_blockpos(fsprefix+"/origin");
		Vec3i offset = ForageSettings.get_vec3i(fsprefix+"/offset");
		Vec3i center_offset = ForageSettings.get_vec3i(fsprefix+"/center");
		origin = origin.add(new Vec3i(offset_mul*offset.getX(), offset_mul*offset.getY(), offset_mul*offset.getZ()));
		
		NBTStructure ret = new NBTStructure(world, origin, center_offset, ForageSettings.get_string(fsprefix+"/base_file"));
		
		ArrayList<String> extensions = new ArrayList<String>(ForageSettings.getByPrefix(fsprefix+"/extensions/"));
		for(String ext : extensions) {
			int rel_x = ForageSettings.get_int(fsprefix+"/extension_relative_positions/" + ext + "/x");
			int rel_y = ForageSettings.get_int(fsprefix+"/extension_relative_positions/" + ext + "/y");
			int rel_z = ForageSettings.get_int(fsprefix+"/extension_relative_positions/" + ext + "/z");
			NBTStructure e = new NBTStructure(world, origin.add(rel_x, rel_y, rel_z), new Vec3i(0,0,0), ext);
			ret.add_extension(e);
		}
		
		if(ForageSettings.get_bool(fsprefix+"/has_multiple_centers")) {
			ret.set_multiple_centers(true, ForageSettings.get_vec3i(fsprefix+"/m_center_offset"), ForageSettings.get_int(fsprefix+"/m_center_amount"));
		}
		
		return ret;		
	}
	
	public List<NBTStructure> get_extensions() {
		return extensions;
	}
	
	public void add_extension(NBTStructure s) {
		extensions.add(s);
	}
	
	@Override
	public void clear() {
		for(NBTStructure e : extensions) e.clear();
		super.clear();
	}
	
	@Override
	public BlockPos get_center() {
		return new BlockPos(origin).add(center_offset);
		
	}
	
	public BlockPos get_center(int index) {
		if(!has_multiple_centers) return get_center();
		
		int mul = index % m_center_amount;
		return new BlockPos(origin).add(center_offset).add(new Vec3i(mul*m_center_offset.getX(), mul*m_center_offset.getY(), mul*m_center_offset.getZ()));
	}
	
	@Override
	public void place() {
		if(!world.isRemote || exists) { //only do this on the server
			for(NBTStructure e : extensions) e.place();
			//if(tmpl==null) { System.out.println("TEMPLATE NOT FOUND"); return; } //After 3hours of debugging i can confirm: this doesn't work.
			
			//Spawn structure
			IBlockState ibs = world.getBlockState(origin);
			
			PlacementSettings plcs = (new PlacementSettings()).setMirror(Mirror.NONE)
					.setRotation(Rotation.NONE).setIgnoreEntities(false).setChunk((ChunkPos) null)
					.setReplacedBlock((Block) null).setIgnoreStructureBlock(!ForageSettings.get_bool("/game/dev"));
			
			tmpl.addBlocksToWorld(world, origin, plcs, 2);
			exists = true;
		}
		else System.out.println("World is remote!");
	}

}
