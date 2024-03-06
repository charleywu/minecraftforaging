package com.arc.forage.gamemaster.events;

import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.L;
import com.arc.forage.tools.StyledText;

public class GM_Countdown implements GMEvent {
	
	private int s;
	private int last_s;
	
	public GM_Countdown(int s) {
		this.s = s;
		this.last_s = s+1;
	}

	@Override
	public boolean process(int tsle) {
		if(last_s!=(s-tsle)) {
			GameMaster.broadcast_title(new StyledText(""+(s-tsle), "white", "bold"), new StyledText(L.get_s("countdown_text"), "gray", "italic"), 5, 10 ,5);
			last_s = (s-tsle);
		}
		return (s-tsle)<=0;
	}

	@Override
	public GMEType get_type() {
		return GMEType.COUNTDOWN;
	}

	@Override
	public String get_status() {
		return "Counting down...";
	}

}
