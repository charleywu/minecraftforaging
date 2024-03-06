package com.arc.forage.gamemaster.events;

import com.arc.forage.gamemaster.GameMaster;

public class GM_StartNextRound implements GMEvent {

	@Override
	public boolean process(int tsle) {
		GameMaster.startRound();
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.STARTNEXTROUND;
	}

	@Override
	public String get_status() {
		return "Starting next round...";
	}

}