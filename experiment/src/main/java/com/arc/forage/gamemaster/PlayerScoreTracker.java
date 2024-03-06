package com.arc.forage.gamemaster;

import java.util.Collection;
import java.util.List;
import java.util.NavigableMap;
import java.util.TreeMap;

import com.arc.forage.settings.ForageSettings;

import net.minecraft.entity.player.EntityPlayer;

public class PlayerScoreTracker {
	private static NavigableMap<String, Integer> map = new TreeMap<String, Integer>();
	private static NavigableMap<String, Integer> round_score = new TreeMap<String, Integer>(); //Contains score of just one round
	
	public static Integer get_score(String pname) {
		if(!map.containsKey(pname)) return 0;
		return map.get(pname);
	}
	
	public static Integer get_roundScore(String pname) {
		if(!round_score.containsKey(pname)) return 0;
		return round_score.get(pname);
	}
	
	public static void remove_entry(String pname) {
		map.remove(pname);
		round_score.remove(pname);
	}
	
	public static void clear() {
		map.clear();
		round_score.clear();
	}
	
	public static void increaseScore(ForagePlayer p, int delta) {
		increaseScore(p.get_player(), delta);
	}
	
	public static void increaseScore(EntityPlayer p, int delta) {
		set_score(p, delta+get_score(p.getName()));
		set_roundScore(p, delta+get_roundScore(p.getName()));
	}
	
	public static void resetPlayerScore(ForagePlayer p) {
		set_score(p, 0);
	}
	
	public static void resetRoundScores() {
		round_score.clear();
	}
	
	public static void resetAllScores() {
		GameMaster.command("/xp -999L @a");
		map.clear();
	}
	
	public static void set_score(ForagePlayer p, int score) {
		set_score(p.get_player(), score);
	}
	
	public static void set_score(EntityPlayer p, int score) {
		map.put(p.getName(), score);
		p.experienceLevel = get_score(p.getName());
		p.addExperienceLevel(0); // This updates the level client-side
	}
	
	public static void set_roundScore(ForagePlayer p, int score) {
		set_roundScore(p, score);
	}
	
	public static void set_roundScore(EntityPlayer p, int score) {
		round_score.put(p.getName(), score);
	}
	
	public static void print_all_scores() {
		String out = "\r\n>> Total scores:";
		for(String s : map.descendingKeySet()) {
			out += "\r\n>>>> "+s+": "+get_score(s)+"  ("+(get_score(s)*ForageSettings.get_int("/game/settings/CentsPerPoint"))+" Cents)";
		}
		System.out.println(out);
		WebAPI.add_info(get_all_scores_html());
	}
	
	public static String get_all_scores_html() {
		String out = "## Total scores:";
		for(String s : map.descendingKeySet()) {
			out += "<br>#### "+s+": "+get_score(s)+"  ("+(get_score(s)*ForageSettings.get_int("/game/settings/CentsPerPoint"))+" Cents)";
		}
		return out;
	}
}
