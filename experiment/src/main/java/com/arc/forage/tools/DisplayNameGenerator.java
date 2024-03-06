package com.arc.forage.tools;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Collections;

public class DisplayNameGenerator {
	
	private static int c = 0;
	
	public static String[] get_n_new_displayNames(int n) {
		String[] ret = new String[n];
		for(int i=0; i<n; i++) ret[i] = get_new_displayName();
		return ret;
	}
	
	public static String get_new_displayName() {
		String s = displayNames[c];
		c = (c+1) % displayNames.length;
		System.out.println("NAME: " + s + " i:" + c);
		return s;
	}
	
	public static void reset() {
		List<String> tmp = Arrays.asList(displayNames);
		Collections.shuffle(tmp);
		
		displayNames = (String[]) tmp.toArray();
		c = 0;
	}
	
	private static String[] displayNames = {
		"YD3T5",
		"C97RY",
		"VWPDV",
		"6EYEV",
		"4O4FJ",
		"SP9EB",
		"G0IKW",
		"LMEFP",
		"UQUGC",
		"PYPR1",
		"AR2H0",
		"RV0D1",
		"WC4AZ",
		"1FUCX",
		"QYEED",
		"JPS4V",
		"MUZSR",
		"SG2PP",
		"DEJBF",
		"03KDF",
		"UHAI7",
		"1QRJY",
		"7E4I7",
		"2Y8N3",
		"COC62",
		"T7HFL",
		"YD1RJ",
		"5BK2O",
		"HEW5J",
		"267BZ",
		"7AB6D",
		"6KKN3",
		"1VS3R",
		"0ETQA",
		"ZYQWY",
		"SAP38",
		"AVCR4",
		"GCFKK",
		"SMHZ7",
		"LRNAY",
		"1L9Q7",
		"RU595",
		"B3D46",
		"VQKVT",
		"YAPB4",
		"FEK45",
		"Z20BK",
		"VRTG2",
		"202A3",
		"77XY5",
		"NY8HL",
		"4437Q",
		"85GSK",
		"J0TJG",
		"A8M7C",
		"GJL1M",
		"EVFIA",
		"WMZGB",
		"N0JQK",
		"YAWSI",
		"IFZQG",
		"A7OYX",
		"4YWXY",
		"0CI55",
		"QR55A",
		"CAFF2",
		"UQAL0",
		"L5HI7",
		"9B35P",
		"AMRHT",
		"E3TZV",
		"CAEG0",
		"X0IR4",
		"2CMZ2",
		"6M2Q8",
		"E5W88",
		"3ADC7",
		"BBOXJ",
		"3CWPO",
		"JVZZ2",
		"DL0WH",
		"FWOXP",
		"OW95T",
		"UFLZF",
		"EZ8XQ",
		"EVXX0",
		"RDW2O",
		"DBW2J",
		"6HZK8",
		"HDD2S",
		"2JJED",
		"APAJ9",
		"E7BJ1",
		"S6P0Z",
		"4PMMN",
		"JWVFC",
		"2EM0M",
		"3CYB0",
		"PE4M0",
		"L9J8N",
		"P1DMQ",
		"L8CUR",
		"X5LK7",
		"0X9DW",
		"RMMKU",
		"3PV2E",
		"WBIAI",
		"G4LIP",
		"DKC1Y",
		"02ZO3",
		"D6ZQA",
		"NP52R",
		"4P4NB",
		"0KQIU",
		"K2FCW",
		"W54K0",
		"4OAWN",
		"QEX56",
		"7G042",
		"LZ1X4",
		"D124W",
		"4OB87",
		"I2D7G",
		"7U84U",
		"0IZW8",
		"F30JB",
		"SBM1S",
		"MWO55"
	};
}
