package com.arc.forage.gamemaster.events;

public interface GMEvent {
	
	enum GMEType {COUNTDOWN, ENDROUND, FIELDPREP, STARTNEXTROUND, TP2FIELD, TP2LOBBY, TP2TUTORIAL, WAITFORPLAYERSREADY, WAITFORSECONDS, RESETSCORE, FORCECHUNKSLOADED, UNLOADCHUNKS};
	
	public GMEType get_type();
	
	public String get_status();
	
	//Returns true if the event needs to be removed from the queue
	//tsle = Time since last event, meaning time the event has been active
	public boolean process(int tsle);
}