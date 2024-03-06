package com.arc.forage.gamemaster.events;

import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.settings.ForageSettings;

public class GM_TP2Lobby implements GMEvent {

private int round_index;

	public GM_TP2Lobby(int round_index) {
		this.round_index = round_index;
	}

	@Override
	public boolean process(int tsle) {
		GameMaster.tp_to_lobby(ForageSettings.get_bool("/game/roundData/"+(round_index+1)+"/soloRound"));
		GameMaster.server.getCommandManager().executeCommand(GameMaster.server, "/time set 11800");
		GameMaster.server.getCommandManager().executeCommand(GameMaster.server, "/gamerule doDaylightCycle false");
		WebAPI.add_info("Round ended.");
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.TP2LOBBY;
	}

	@Override
	public String get_status() {
		return "Teleporting to lobby...";
	}
}
