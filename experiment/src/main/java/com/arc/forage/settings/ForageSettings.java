package com.arc.forage.settings;

import java.util.Collection;
import java.util.NavigableMap;
import java.util.TreeMap;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;

public class ForageSettings {
	private static NavigableMap<String, String> map = new TreeMap<String, String>();
	
	public static Collection<String> getByPrefix(String prefix) {
	    return map.subMap( prefix, prefix + Character.MAX_VALUE ).values(); //Since this is also a sortedMap, elements will be sorted.
	}
	
	public static void clear() {
		map.clear();
	}
	
	public static int get_int(String key) {
		if(!map.containsKey(key)) return 0;
		
		try { return Integer.parseInt(map.get(key)); }
		catch (NumberFormatException e) { return 0; }
	}
	
	public static float get_float(String key) {
		if(!map.containsKey(key)) return 0;
		
		try { return Float.parseFloat(map.get(key)); }
		catch (NumberFormatException e) { return 0; }
	}
	
	public static String get_string(String key) {
		if(!map.containsKey(key)) return "";
		return map.get(key);
	}
	
	public static BlockPos get_blockpos(String prefix) {
		return new BlockPos(get_int(prefix+"/x"), get_int(prefix+"/y"), get_int(prefix+"/z"));
	}
	
	public static Vec3i get_vec3i(String prefix) {
		return new Vec3i(get_int(prefix+"/x"), get_int(prefix+"/y"), get_int(prefix+"/z"));
	}
	
	public static boolean get_bool(String key) {
		if(!map.containsKey(key)) return false;
		return Boolean.parseBoolean(map.get(key));
	}
	
	public static boolean contains_key(String key) {
		if (!map.containsKey(key)) {
			System.out.println(">> ERR: Attempted to access key " + key + ", which does not exist.");
			return false;
		}
		return true;
	}
	
	public static void add(String key, String val) {
		//System.out.println("Key added: " + key + " : " +  val);
		map.put(key, val);
	}
}