package com.arc.forage.gamemaster.events;

import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.settings.ForageSettings;

public class GM_TP2Field implements GMEvent {

	private int round_index;

	public GM_TP2Field(int round_index) {
		this.round_index = round_index;
	}

	@Override
	public boolean process(int tsle) {
		int game_time = ForageSettings.get_int("/game/roundData/"+round_index+"/time_limit");
		game_time *= 20; //20 ticks per second
		game_time = 12700  - game_time; //13000 is sunset
		GameMaster.server.getCommandManager().executeCommand(GameMaster.server, "/time set "+game_time);
		GameMaster.server.getCommandManager().executeCommand(GameMaster.server, "/gamerule doDaylightCycle true");

		GameMaster.tp_to_field(ForageSettings.get_bool("/game/roundData/"+round_index+"/soloRound"));
		WebAPI.add_info("Round in progress.");
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.TP2FIELD;
	}

	@Override
	public String get_status() {
		return "Teleporting to field...";
	}

}
