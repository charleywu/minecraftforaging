package com.arc.forage.gamemaster;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.arc.forage.events.KeepBlocks;
import com.arc.forage.gamemaster.events.GMEvent;
import com.arc.forage.gamemaster.events.GMEvent.GMEType;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.networking.Network;
import com.arc.forage.gamemaster.events.GM_Countdown;
import com.arc.forage.gamemaster.events.GM_EndRound;
import com.arc.forage.gamemaster.events.GM_FieldPrep;
import com.arc.forage.gamemaster.events.GM_ForceChunksLoaded;
import com.arc.forage.gamemaster.events.GM_StartNextRound;
import com.arc.forage.gamemaster.events.GM_TP2Field;
import com.arc.forage.gamemaster.events.GM_TP2Lobby;
import com.arc.forage.gamemaster.events.GM_TP2Tutorial;
import com.arc.forage.gamemaster.events.GM_UnloadChunks;
import com.arc.forage.gamemaster.events.GM_WaitForPlayersReady;
import com.arc.forage.gamemaster.events.GM_WaitForSeconds;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.settings.ForageSettingsParser;
import com.arc.forage.structures.Field;
import com.arc.forage.structures.JSONStructure;
import com.arc.forage.structures.NBTStructure;
import com.arc.forage.structures.Structure;
import com.arc.forage.tools.DisplayNameGenerator;
import com.arc.forage.tools.FieldTable;
import com.arc.forage.tools.StyledText;
import com.arc.forage.networking.messages.*;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.management.PlayerList;
import net.minecraft.util.EnumParticleTypes;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.TickEvent;
import net.minecraftforge.fml.common.gameevent.PlayerEvent.PlayerLoggedInEvent;
import net.minecraftforge.fml.common.gameevent.PlayerEvent.PlayerLoggedOutEvent;
import net.minecraftforge.fml.common.gameevent.TickEvent.ServerTickEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;

@EventBusSubscriber
public class GameMaster {

	public enum GAMESTATES {INLOBBY, INTUTORIAL, INFIELD_SEPARATE, INFIELD_SOCIAL};
	public static GAMESTATES currentstate = GAMESTATES.INLOBBY;

	public static MinecraftServer server;
	static int tick_c = 0;
	static final int tps = 20;
	static Queue<GMEvent> event_q = new ConcurrentLinkedQueue<GMEvent>();
	static int time_at_last_event = 0;
	static int ticks_at_round_start = 0;
	static boolean cleared=false;
	static boolean tutorial_completed = false;

	static List<Structure> all_structures = new ArrayList<Structure>();
	static List<NBTStructure> lobbies = new ArrayList<NBTStructure>();
	static List<NBTStructure> tutorials = new ArrayList<NBTStructure>();
	static NBTStructure social_lobby;
	static List<Field> fields = new ArrayList<Field>();
	public static List<ForagePlayer> players = new ArrayList<ForagePlayer>(); //TODO set private

	static int round_index = 0;
	//static ArrayList<String> rounds;

	public static int player_amount;

	public static void init(MinecraftServer s, String[] args) {
		if(s.getEntityWorld().isRemote) return;

		ForageSettingsParser.parse(args);

		round_index = 0;
		server = s;

		updateBlockHardnesses_forAll();
		KeepBlocks.set_active(!ForageSettings.get_bool("/game/dev"));


		//Exec server_start_commands
		for(String com : ForageSettings.getByPrefix("/server_start_commands/")) {
			server.getCommandManager().executeCommand(server, com);
		}

		//Get player amount
		player_amount = ForageSettings.get_int("/game/settings/player_amount");

		//Clear all structures that were previously on this server. This might be replaced by the /save-off command on dedicated servers
		for(Structure st : recoverStructures()) st.clear();
		cleared = true;

		//Place lobbies
		{
			for(int i=0; i<player_amount; i++) {
				addLobby(NBTStructure.fromSettings(server.getEntityWorld(), "/structures/lobby", i));
			}
		}

		//Place social lobby
		{
			addSocialLobby(NBTStructure.fromSettings(server.getEntityWorld(), "/structures/social_lobby", 0));
		}

		//Place tutorials
		if(ForageSettings.get_bool("/game/settings/tutorial")) {
			for(int i=0; i<player_amount; i++) {
				addTutorial(NBTStructure.fromSettings(server.getEntityWorld(), "/structures/tutorial_"+ForageSettings.get_string("/game/metaData/0/smoothBlockType"), i));
			}
		}
		
		if(ForageSettings.get_bool("/game/settings/forceChunkLoad")) ChunkLoader.init(server.getEntityWorld());

		//TODO: Only do this if Web API enabled in settings
		(new WebAPI()).init();
		
		tick_c = 0;
		ticks_at_round_start = 0;
	}

	public static void reconfigure(String[] args) {
		if(currentstate!=GAMESTATES.INLOBBY && !ForageSettings.get_bool("/game/dev")) {
			broadcast("Can only reconfigure when in lobby. Run /egame first.");
			return;
		}
		event_q.clear();
		round_index = 0;
		tutorial_completed = false;
		DisplayNameGenerator.reset();
		PlayerScoreTracker.resetAllScores();
		PlayerScoreTracker.clear();
		DataLogger.uninit();
		if(!ForageSettings.get_bool("/game/dev")) tp_to_lobby(true);
		for(Structure s : all_structures) s.clear();
		all_structures.clear();
		lobbies.clear();
		tutorials.clear();
		fields.clear();
		DataLogger.uninit();
		init(server, args);
	}

	public static void endGame() {
		WebAPI.set_status("Game finished.");
		
		broadcast_styled(new StyledText(L.get_s("experiment_finished"), "green", new String[] {"bold", "underlined"}));
		for(ForagePlayer p : players) {
			whisper_styled(new StyledText(L.get_s("congratulations"),"white","italic").append(L.get_s("you_had_total_score")).append(""+PlayerScoreTracker.get_score(p.get_player().getName()), "red").append(L.get_s("points")), p.get_player());
		}
		round_index = 0;
		tutorial_completed = false;
		DataLogger.uninit();
		System.out.println("\r\n\r\n\r\n");
		System.out.println(">> Experiment has finished <<");
		PlayerScoreTracker.print_all_scores();
	}

	public static void force_endGame() {
		WebAPI.set_status("No session in progress.");
		tp_to_lobby(true);
		event_q.clear();
		round_index = 0;
		tutorial_completed = false;
		DataLogger.uninit();
	}

	public static void addField(Field f) {
		if(!f.exists()) f.place();
		fields.add(f);
		all_structures.add(f);
	}

	public static void addTutorial(NBTStructure t) {
		if(!t.exists()) t.place();
		tutorials.add(t);
		all_structures.add(t);
	}

	public static void addLobby(NBTStructure l) {
		if(!l.exists()) l.place();
		lobbies.add(l);
		all_structures.add(l);
	}

	public static void addSocialLobby(NBTStructure sl) {
		if(!sl.exists()) sl.place();
		if(social_lobby!=null && social_lobby.exists()) social_lobby.clear();
		social_lobby = sl;
		all_structures.add(sl);
	}

	public static void startRound() {		
		if(!ForageSettings.contains_key("/game/roundData/"+round_index+"/round")) {
			endGame();
			return; //Round doesnt exist.
		}

		time_at_last_event = server_time();

		if(ForageSettings.get_bool("/game/settings/tutorial") && !tutorial_completed) {
			//Tutorial ROund stack:
			push_event(new GM_WaitForSeconds(1));
			push_event(new GM_TP2Tutorial());
			push_event(new GM_WaitForSeconds(3));
			push_event(new GM_WaitForPlayersReady(players));
			push_event(new GM_TP2Lobby(-1));
			push_event(new GM_EndRound(false));
			push_event(new GM_WaitForSeconds(1));
			push_event(new GM_StartNextRound());
			tutorial_completed = true;
		}
		else {
			WebAPI.set_status("Current round: "+round_index+" out of 17"); //TODO: Make 17 dynamic
			DataLogger.init(ForageSettings.get_string("/game/name"), round_index);
			//Regular Round stack:
			push_event(new GM_WaitForSeconds(1));
			push_event(new GM_FieldPrep(round_index));
			push_event(new GM_WaitForSeconds(1));
			push_event(new GM_WaitForPlayersReady(players));
			push_event(new GM_Countdown(ForageSettings.get_int("/game/settings/round_start_countdown")));
			push_event(new GM_TP2Field(round_index));
			push_event(new GM_WaitForSeconds(ForageSettings.get_int("/game/roundData/"+round_index+"/time_limit")-ForageSettings.get_int("/game/settings/round_end_countdown")));
			push_event(new GM_Countdown(ForageSettings.get_int("/game/settings/round_end_countdown")));
			if(ForageSettings.get_bool("/game/settings/forceChunkLoad")) push_event(new GM_ForceChunksLoaded(players));
			push_event(new GM_TP2Lobby(round_index));
			push_event(new GM_WaitForSeconds(1));
			push_event(new GM_EndRound());
			push_event(new GM_WaitForSeconds(4));
			if(ForageSettings.get_bool("/game/settings/forceChunkLoad")) push_event(new GM_UnloadChunks());
			push_event(new GM_StartNextRound());
		}
	}

	public static void endRound(boolean increase_round_index) {
		for(ForagePlayer p : players) {
			// You had a score of X
			StyledText score_anouncement = new StyledText(L.get_s("you_found")).append(""+PlayerScoreTracker.get_roundScore(p.get_player().getName()), "red").append(L.get_s("rewards_that_round"));
			if(!ForageSettings.get_bool("/game/roundData/"+round_index+"/trainingRound")) score_anouncement.append(L.get_s("total")).append(""+PlayerScoreTracker.get_score(p.get_player().getName()), "red").append(")");
			if(increase_round_index) whisper_styled(score_anouncement, p.get_player());
		}

		if(!increase_round_index || ForageSettings.get_bool("/game/roundData/"+round_index+"/trainingRound")) {
			PlayerScoreTracker.resetAllScores();
		}
		
		PlayerScoreTracker.resetRoundScores();

		for(Field f : fields) {
			f.clear();
			all_structures.remove(f);
		}

		DataLogger.uninit();
		fields.clear();
		if(increase_round_index) round_index++;
		if(ForageSettings.get_bool("/game/settings/loop") && !ForageSettings.contains_key("/game/roundData/"+(round_index+1)+"/round")) round_index = 0;
	}

	private static List<Structure> recoverStructures() {
		List<Structure> ret = new ArrayList<Structure>();
		try {
			FileInputStream fi = new FileInputStream(new java.io.File("objects_recover.txt"));
			ObjectInputStream oi = new ObjectInputStream(fi);
			int size = oi.readInt();
			for(int i=0; i<size; i++) ret.add(new Structure(server.getEntityWorld(), (JSONStructure)oi.readObject()));

			oi.close();
			fi.close();
		} catch (Exception e) {
			System.out.println(">> ERR: Could not read objects_recover.txt");
		}
		return ret;
	}

	public static void broadcast_status() {
		if(event_q.isEmpty()) broadcast("No game in progress.");
		else broadcast(event_q.peek().get_status());
	}

	public static void push_event(GMEvent ev) {
		if(event_q.isEmpty()) time_at_last_event = server_time();
		event_q.add(ev);
	}

	public static void tp_to_field(boolean separate) {
		assign_new_random_displaynames();
		ticks_at_round_start = tick_c;
		if(separate) {
			currentstate = GAMESTATES.INFIELD_SEPARATE;
			for(ForagePlayer p : players) {
				tp_to(p.get_player(), fields.get(p.get_id()).get_random(), true);
			}
		}
		else {
			currentstate = GAMESTATES.INFIELD_SOCIAL;
			for(ForagePlayer p : players) {
				tp_to(p.get_player(), fields.get(0).get_random(), true);
			}
		}
	}

	public static void tp_to_lobby(boolean separate) {
		clear_displaynames();
		currentstate = GAMESTATES.INLOBBY;
		if(separate) {
			for(ForagePlayer p : players) tp_to(p.get_player(), lobbies.get(p.get_id()).get_center());
		}
		else {
			for(ForagePlayer p : players) tp_to(p.get_player(), social_lobby.get_center(p.get_id()));
		}
	}

	public static void tp_to_tutorial() {
		clear_displaynames();
		currentstate = GAMESTATES.INTUTORIAL;
		for(ForagePlayer p : players) tp_to(p.get_player(), tutorials.get(p.get_id()).get_center());
	}

	public static void broadcast(String s) {
		command("say " + s);
	}

	public static void broadcast_styled(StyledText txt) {
		command("/tellraw @a " + txt.toJSON());
	}

	public static void whisper_styled(StyledText txt, EntityPlayer p) {
		command("/tellraw "+ p.getName() +" " + txt.toJSON());
	}

	public static void broadcast_title(StyledText txt, StyledText subtitle, int fade_in_ticks, int duration_ticks, int fade_out_ticks) {
		command("/title @a times " + fade_in_ticks + " " + duration_ticks + " " + fade_out_ticks);
		command("/title @a title " + txt.toJSON());
		command("/title @a subtitle " + subtitle.toJSON());
	}

	public static void command(String s) {
		server.getCommandManager().executeCommand(server, s);
	}

	public static void updateBlockHardnesses(EntityPlayer p) {
		Network.INSTANCE.sendTo(new SetBlockHardness(ModBlocks.magicMelon ,ForageSettings.get_float("/constants/melon_hardness")), (EntityPlayerMP) p);
		Network.INSTANCE.sendTo(new SetBlockHardness(ModBlocks.magicPumpkin ,ForageSettings.get_float("/constants/pumpkin_hardness")), (EntityPlayerMP) p);
	}

	public static void updateBlockHardnesses_forAll() {
		Network.INSTANCE.sendToAll(new SetBlockHardness(ModBlocks.magicMelon ,ForageSettings.get_float("/constants/melon_hardness")));
		Network.INSTANCE.sendToAll(new SetBlockHardness(ModBlocks.magicPumpkin ,ForageSettings.get_float("/constants/pumpkin_hardness")));
	}

	//This event is fired only by the server thread.
	@SubscribeEvent
	public static void OnPlayerConnect(PlayerLoggedInEvent event) {
		updateBlockHardnesses((EntityPlayerMP) event.player);
		event.player.setPositionAndUpdate(0, 4, 0);
		if(players.size()<player_amount) {
			ForagePlayer p = new ForagePlayer(event.player);
			players.add(p);
			p.get_player().experienceLevel = PlayerScoreTracker.get_score(p.get_player().getName());
			if(currentstate == GAMESTATES.INTUTORIAL) tp_to(event.player, tutorials.get(p.get_id()).get_center());
			else if(currentstate == GAMESTATES.INFIELD_SEPARATE) tp_to(event.player, fields.get(p.get_id()).get_center());
			else if(currentstate == GAMESTATES.INFIELD_SOCIAL) tp_to(event.player, fields.get(0).get_center());
			else tp_to(event.player, lobbies.get(p.get_id()).get_center());
		}
		for(String c : ForageSettings.getByPrefix("/onconnect_commands")) {
			command(c);
		}
	}

	// Does the same thing as OnPlayerDisconnect but for a passed username
	public static void Exclude(String user) {
		for(ForagePlayer p : players) {
			if(p.get_player().getName()==user) {
				tp_to(p.get_player(), new BlockPos(0,4,0));
				PlayerVisTracker.remove_entry(p.get_player().getName());
				p.Destroy();
				players.remove(p);
				System.out.println("PLAYER REMOVED");
				break;
			}
		}
	}

	@SubscribeEvent
	public static void OnPlayerDisconnect(PlayerLoggedOutEvent event) {
		for(ForagePlayer p : players) {
			if(p.get_player()==event.player) {
				if(currentstate==GAMESTATES.INTUTORIAL) PlayerScoreTracker.resetPlayerScore(p);
				PlayerVisTracker.remove_entry(p.get_player().getName());
				p.Destroy();
				players.remove(p);
				System.out.println("PLAYER REMOVED");
				break;
			}
		}
	}

	//This event is fired only by the server thread.
	@SubscribeEvent
	public static void OnWorldSave(WorldEvent.Save event) {
		if(!cleared) return;
		FileOutputStream f;
		try {
			f = new FileOutputStream(new java.io.File("objects_recover.txt"));
			ObjectOutputStream o = new ObjectOutputStream(f);

			o.writeInt(all_structures.size());
			for(Structure s : all_structures) if(s.exists()) o.writeObject(s.toJSON());

			o.close();
			f.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void tp_to(EntityPlayer player, BlockPos pos) {
		player.setPositionAndUpdate(pos.getX()+0.5f, pos.getY(), pos.getZ()+0.5f);
	}
	
	private static void tp_to(EntityPlayer player, BlockPos pos, boolean randomizeYaw) {
		if(!randomizeYaw) tp_to(player, pos);
		else {
			player.setPositionAndRotation(pos.getX()+0.5f, pos.getY(), pos.getZ()+0.5f, 360f*(float)Math.random(), 0.0f);
			// Not running "AndUpdate" will cause a "Player moved wrongly" warning. That's why we tp again here.
			player.setPositionAndUpdate(pos.getX()+0.5f, pos.getY(), pos.getZ()+0.5f);
		}
	}

	//Runs only ons server thread (allegedly)
	@SubscribeEvent
	public static void OnServerTick(ServerTickEvent event) {
		if(event.phase == TickEvent.Phase.START) { //Otherwise we get twice as many ticks.
			tick_c++;
			if(!event_q.isEmpty() && event_q.peek().process(tsle())) remove_event();
			if(currentstate==GAMESTATES.INFIELD_SEPARATE || currentstate==GAMESTATES.INFIELD_SOCIAL) DataLogger.log_players(players);
			
			// Keep foodlevel low to disable double-tap sprinting
			if(ForageSettings.get_bool("/game/settings/const_foodlevel_1")) {
				for(ForagePlayer p : players) {
					p.get_player().getFoodStats().setFoodLevel(6);
				}
			}
		}
		if(WebAPI.has_unprocessed_Requests()) {
			WebAPI.process_requests();
		}
	}

	public static int server_time() {
		return tick_c/tps;
	}

	public static float server_time_f() {
		return (float)tick_c/tps;
	}

	public static float time_since_round_start_f() {
		return (float)(tick_c - ticks_at_round_start) / (float)tps;
	}

	public static int tsle() {
		return server_time()-time_at_last_event;
	}

	private static void remove_event() {
		time_at_last_event = server_time();
		event_q.remove();
		if(!event_q.isEmpty()) System.out.println(event_q.peek().get_status());
	}

	public static ForagePlayer getFP(EntityPlayer p) {
		for(ForagePlayer ret : players) {
			if(ret.get_player()==p) return ret;
		}
		return null;
	}
	
	public static void assign_new_random_displaynames() {
		Network.INSTANCE.sendToAll(new SetPlayerDisplayNames(players, DisplayNameGenerator.get_n_new_displayNames(players.size())));
	}
	
	public static void clear_displaynames() {
		String[] empty_names = new String[players.size()];
		for(int i=0; i<empty_names.length; i++) empty_names[i] = "";
		Network.INSTANCE.sendToAll(new SetPlayerDisplayNames(players, empty_names));
	}
}
