package com.arc.forage;

import net.minecraft.init.Blocks;
import net.minecraftforge.common.ForgeChunkManager;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventHandler;
import net.minecraftforge.fml.common.Mod.Instance;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPostInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.event.FMLServerStartedEvent;
import net.minecraftforge.fml.common.event.FMLServerStartingEvent;
import net.minecraftforge.fml.common.event.FMLServerStoppedEvent;
import net.minecraftforge.fml.relauncher.Side;

import org.apache.logging.log4j.Logger;

import com.arc.forage.commands.blockbreak;
import com.arc.forage.commands.egame;
import com.arc.forage.commands.exclude;
import com.arc.forage.commands.get;
import com.arc.forage.commands.getscores;
import com.arc.forage.commands.reconfigure;
import com.arc.forage.commands.set;
import com.arc.forage.commands.setup;
import com.arc.forage.commands.sgame;
import com.arc.forage.commands.status;
import com.arc.forage.events.KeepBlocks;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.Network;
import com.arc.forage.networking.messagehandlers.ReportVisiblePlayersHandler;
import com.arc.forage.networking.messagehandlers.SetBlockHardnessHandler;
import com.arc.forage.networking.messagehandlers.SetPlayerDisplayNamesHandler;
import com.arc.forage.networking.messages.ReportVisiblePlayers;
import com.arc.forage.networking.messages.SetBlockHardness;
import com.arc.forage.networking.messages.SetPlayerDisplayNames;
import com.arc.forage.settings.ForageSettingsParser;

@Mod(modid = Forage.MODID, name = Forage.NAME, version = Forage.VERSION)
public class Forage
{
	@Mod.Instance
	public static Forage INSTANCE;
    public static final String MODID = "forage";
    public static final String NAME = "Forage Mod";
    public static final String VERSION = "1.2.4";
    public static final String ACCEPTED_MINECRAFT_VERSIONS = "[1.12.2]";

    private static Logger logger;

    @EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
    	ForgeChunkManager.setForcedChunkLoadingCallback(INSTANCE, new ChunkLoadingHandler());
        logger = event.getModLog();
        ModBlocks.init();
    }

    @EventHandler
    public void init(FMLInitializationEvent event)
    {
    	int msg_id=0;
    	Network.INSTANCE.registerMessage(SetBlockHardnessHandler.class, SetBlockHardness.class, msg_id++, Side.CLIENT);
    	Network.INSTANCE.registerMessage(SetPlayerDisplayNamesHandler.class, SetPlayerDisplayNames.class, msg_id++, Side.CLIENT);
    	Network.INSTANCE.registerMessage(ReportVisiblePlayersHandler.class, ReportVisiblePlayers.class, msg_id++, Side.SERVER);
    }
    
    @EventHandler
    public void postinit(FMLPostInitializationEvent event)
    {
    	//.
    }
    
    @EventHandler
    public void start(FMLServerStartingEvent event)
    {
    	event.registerServerCommand(new sgame());
    	event.registerServerCommand(new egame());
    	event.registerServerCommand(new status());
    	event.registerServerCommand(new reconfigure());
    	event.registerServerCommand(new blockbreak());
    	event.registerServerCommand(new exclude());
    	event.registerServerCommand(new getscores());
    	event.registerServerCommand(new get());
    	event.registerServerCommand(new set());
    	event.registerServerCommand(new setup());
    }
    
    @EventHandler
	public void serverStarted(FMLServerStartedEvent event) {
    	GameMaster.init(FMLCommonHandler.instance().getMinecraftServerInstance(), new String[] {});
    	WebAPI.set_status("Server Started.");
	}
    
    @EventHandler
    public void serverStoppend(FMLServerStoppedEvent event) {
    	WebAPI.set_error("Server Offline.");
    }
}
