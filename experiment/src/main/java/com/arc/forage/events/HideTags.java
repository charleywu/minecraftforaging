package com.arc.forage.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.arc.forage.Forage;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.Network;
import com.arc.forage.networking.messages.ReportVisiblePlayers;
import com.arc.forage.networking.messages.SetBlockHardness;
import com.arc.forage.settings.ForageSettings;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.EntityRenderer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraftforge.client.event.RenderLivingEvent;
import net.minecraftforge.client.event.RenderPlayerEvent;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.Side;

@Mod.EventBusSubscriber(value = Side.CLIENT, modid = Forage.MODID)
public class HideTags {
	
	public static boolean hideTags = true;
	
	@SubscribeEvent
	public static void onRenderPlayerSpecial(RenderLivingEvent.Specials.Pre event) {
		event.setCanceled(hideTags);
	}
}
