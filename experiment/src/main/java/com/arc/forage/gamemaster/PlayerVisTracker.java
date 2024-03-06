package com.arc.forage.gamemaster;

import java.util.Collection;
import java.util.NavigableMap;
import java.util.TreeMap;

public class PlayerVisTracker {
	private static NavigableMap<String, String> map = new TreeMap<String, String>();
	
	public static String get_vis(String pname) {
		if(!map.containsKey(pname)) return "";
		return map.get(pname);
	}
	
	public static void remove_entry(String pname) {
		map.remove(pname);
	}
	
	public static void set_vis(String subject_name, String targets) {
		map.put(subject_name, targets);
	}
}
