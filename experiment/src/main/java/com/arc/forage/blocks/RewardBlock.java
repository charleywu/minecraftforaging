package com.arc.forage.blocks;

import java.util.List;

import com.arc.forage.Forage;
import com.arc.forage.gamemaster.DataLogger;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.PlayerScoreTracker;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.structures.Field;
import com.arc.forage.tools.PseudoRandomUnitSpherePointGenerator;

import mcp.MethodsReturnNonnullByDefault;
import net.minecraft.block.Block;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.properties.PropertyBool;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.particle.Particle;
import net.minecraft.client.particle.ParticleSplash;
import net.minecraft.util.EnumParticleTypes;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec2f;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;
import net.minecraftforge.common.util.Constants.WorldEvents;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityAreaEffectCloud;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.item.EntityFireworkRocket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.projectile.EntityPotion;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.potion.PotionEffect;
import net.minecraft.potion.PotionType;
import net.minecraft.potion.PotionUtils;
public class RewardBlock extends Block {

	public RewardBlock(String name, Material material) {
		super(material);
		setUnlocalizedName(Forage.MODID + "." + name);
		setRegistryName(name);
	}
	
	@Override
	public boolean removedByPlayer(IBlockState state, World world, BlockPos pos, EntityPlayer player, boolean willHarvest) {		
		// RUN ON SERVER
		if(!world.isRemote && world.isBlockPowered(pos.down())) {
			// increase score
			PlayerScoreTracker.increaseScore(player, 1);
			
			if(ForageSettings.get_bool("/game/settings/player_splash")) {
				spawnCloud(world, player.posX, player.posY, player.posZ, EnumParticleTypes.SPELL, 2.0f, 100, 3);
			}
			
			if(ForageSettings.get_bool("/game/settings/plusone_indicator")) {
				WorldServer ws = (WorldServer)world;
				ws.spawnParticle(EnumParticleTypes.VILLAGER_ANGRY, true, player.posX, player.posY+2.0f, player.posZ, 1, 0d, 0d, 0d, 0d, 0);
			}
			
			if(ForageSettings.get_bool("/game/settings/splash")) {
				spawnCloud(world, pos.getX()+0.5f, pos.getY()+0.5f, pos.getZ()+0.5f, EnumParticleTypes.SPELL, 2.0f, 100, 3);
			}
			
			if(ForageSettings.get_bool("/game/settings/fireworks")) {
				for(String suffix : ForageSettings.getByPrefix("/firework_commands"))
					GameMaster.command("/summon fireworks_rocket "+pos.getX()+" "+pos.getY()+" "+pos.getZ()+" "+suffix);
			}
		}
		
		//Log the break event only if it is a "legitimate" RewardBlock (meaning outside of tutorial)
		if(!world.isRemote && (GameMaster.currentstate==GameMaster.GAMESTATES.INFIELD_SEPARATE || GameMaster.currentstate==GameMaster.GAMESTATES.INFIELD_SOCIAL)) {
			DataLogger.log_block(player, pos, world.isBlockPowered(pos.down()));
		}
		
		return super.removedByPlayer(state, world, pos, player, willHarvest);
	}
	
	// This is the "new" way of spawning clouds. They spawn in a hemisphere at pos and with radius. We select "amount" pseudo-random locations and spawn "density" particles at each.
	// This way of doing it makes the particles visible beyond any render distance limits.
	// You need to call this *SERVER SIDE*, ie when world.isRemote is false.
	public static void spawnCloud(World world, double posX, double posY, double posZ, EnumParticleTypes part, float radius, int amount, int density) {
		List<Vec3d> ps = PseudoRandomUnitSpherePointGenerator.get_pointlist(amount);
		for(Vec3d v : ps) {
			// We spawn the particles on the world server, and *for each location* we send a packet to the clients. yikes...
			// (Optional) TODO: Send a single packet to clients instead and let them spawn the cloud...
			WorldServer ws = (WorldServer)world;
			ws.spawnParticle(part, true, posX + radius*v.x, posY + radius*Math.abs(v.y), posZ + radius*v.z, density, 0d, 0d, 0d, 0d, 0);
        }
	}
	
	// This is the "old" / deprecated way of spawning particles. Using the AreaEffectClouds are much easier to manage and customize,
	// but the particles have a strict, hardcoded render distance. This is why we stopped using this.
	// This needs to be called *SERVER SIDE*, ie when world.isRemote is false.
	/*
	public static void spawnCloud_dep(World world, double posX, double posY, double posZ, EnumParticleTypes part, float radius, int ticks) {		
		EntityAreaEffectCloud eaec = new EntityAreaEffectCloud(world, posX, posY, posZ);		
    	eaec.setColor(1992905);
		eaec.setParticle(part);
        eaec.setRadius(radius);
        eaec.setDuration(ticks);
        eaec.setWaitTime(0);
        world.spawnEntity(eaec);
	}
	
	public static void spawnCloud_dep(World world, BlockPos pos) {		
		EntityAreaEffectCloud eaec = new EntityAreaEffectCloud(world, pos.getX()+0.5f, pos.getY()+0.5f, pos.getZ()+0.5f);
    	eaec.setColor(1992905);
        eaec.setRadius(0.5F);
        eaec.setDuration(3);
        eaec.setWaitTime(4);
        float radius = 3;
        eaec.setRadiusPerTick((radius - eaec.getRadius()) / (float)eaec.getDuration());

        world.spawnEntity(eaec);
	}
	*/
}
