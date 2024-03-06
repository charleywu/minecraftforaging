package com.arc.forage.gamemaster;

import com.arc.forage.settings.ForageSettings;

//Localization
public class L {
	public static String get_s(String key) {
		if(!ForageSettings.contains_key("/localization/" + get_loc() + "/" + key)) {
			return key;
		}
		return ForageSettings.get_string("/localization/" + get_loc() + "/" + key);
	}
	
	public static String get_loc() {
		return ForageSettings.get_string("/game/metaData/0/language");
	}
}
