package com.arc.forage.gamemaster.events;

import java.util.List;

import com.arc.forage.gamemaster.ForagePlayer;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.L;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.tools.StyledText;

public class GM_WaitForPlayersReady implements GMEvent {
	
	List<ForagePlayer> players;
	int last_ready_players = 0;
	boolean first_process = true;
	
	private void broadcast(int readyplayers) {
		StyledText st = new StyledText(L.get_s("ready_players")).append(readyplayers+"/"+GameMaster.player_amount, "aqua", "bold");
		GameMaster.broadcast_styled(st);
	}
	
	public GM_WaitForPlayersReady(List<ForagePlayer> players) {
		this.players = players;
	}

	@Override
	public boolean process(int tsle) {
		if(first_process) {
			WebAPI.add_info("Waiting for Players to be ready...");
			GameMaster.broadcast_styled(new StyledText(L.get_s("proceed_to")).append(L.get_s("green_platform"), "green"));
			first_process = false;
			return false;
		}
		
		int ready_players = 0;
		for(ForagePlayer p : players) {
			if(p.check_ready()) ready_players++;
		}
		if(ready_players!=last_ready_players && ready_players<GameMaster.player_amount) {
			this.broadcast(ready_players);
			last_ready_players = ready_players;
		}
		return ready_players >= GameMaster.player_amount;
	}

	@Override
	public GMEType get_type() {
		return GMEType.WAITFORPLAYERSREADY;
	}

	@Override
	public String get_status() {
		return "Waiting for players to be ready... ";
	}

}
