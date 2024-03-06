package com.arc.forage.gamemaster.events;

import com.arc.forage.gamemaster.GameMaster;

public class GM_EndRound implements GMEvent {
	
	private boolean increase_round_index;
	
	public GM_EndRound(boolean increase_round_index) {
		this.increase_round_index = increase_round_index;
	}
	
	public GM_EndRound() {
		this.increase_round_index = true;
	}

	@Override
	public boolean process(int tsle) {
		GameMaster.endRound(increase_round_index);
		
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.ENDROUND;
	}

	@Override
	public String get_status() {
		return "Ending Round...";
	}

}