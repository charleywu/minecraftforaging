package com.arc.forage.init;

import com.arc.forage.blocks.MagicMelon;
import com.arc.forage.blocks.MagicPumpkin;
import com.arc.forage.blocks.RewardBlock;

import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.model.ModelLoader;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

@EventBusSubscriber
public class ModBlocks {

	public static Block magicPumpkin;
	public static Block magicMelon;
	
	public static void init() {
	  magicPumpkin = new MagicPumpkin();
	  magicMelon = new MagicMelon();
	}
	
	@SubscribeEvent
	public static void registerBlocks(RegistryEvent.Register<Block> event) {
	  event.getRegistry().registerAll(magicPumpkin);
	  event.getRegistry().registerAll(magicMelon);
	}
	
	@SubscribeEvent
	public static void registerItemBlocks(RegistryEvent.Register<Item> event) {
	  event.getRegistry().registerAll(new ItemBlock(magicPumpkin).setRegistryName(magicPumpkin.getRegistryName()));
	  event.getRegistry().registerAll(new ItemBlock(magicMelon).setRegistryName(magicMelon.getRegistryName()));
	}
	
	@SubscribeEvent
	public static void registerRenders(ModelRegistryEvent event) {
	  registerRender(Item.getItemFromBlock(magicPumpkin));
	  registerRender(Item.getItemFromBlock(magicMelon));
	}
	
	public static void registerRender(Item item) {
	  ModelLoader.setCustomModelResourceLocation(item, 0, new ModelResourceLocation( item.getRegistryName(), "inventory"));
	}
}
