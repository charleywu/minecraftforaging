package com.arc.forage.gamemaster.events;

public class GM_WaitForSeconds implements GMEvent {
	
	private int s;
	
	public GM_WaitForSeconds(int s) {
		this.s = s;
	}

	@Override
	public boolean process(int tsle) {
		return tsle>=s;
	}

	@Override
	public GMEType get_type() {
		return GMEType.WAITFORSECONDS;
	}

	@Override
	public String get_status() {
		return "Waiting for seconds...";
	}

}
