package com.arc.forage.gamemaster.events;

import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.L;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.tools.StyledText;

public class GM_TP2Tutorial implements GMEvent {
	
	public GM_TP2Tutorial() {	}

	@Override
	public boolean process(int tsle) {
		GameMaster.broadcast_styled(new StyledText(L.get_s("tutorial_started"), "green"));
		GameMaster.tp_to_tutorial();
		WebAPI.add_info("Tutorial in progress.");
		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.TP2TUTORIAL;
	}

	@Override
	public String get_status() {
		return "Teleporting to tutorial...";
	}

}