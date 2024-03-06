package com.arc.forage.settings;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Array;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.json.JSONArray;
import org.json.JSONObject;

public class ForageSettingsParser {


	public static void parse(String[] args) {
		try {
			ForageSettings.clear();

			String content = readFile("forage_settings.txt", StandardCharsets.UTF_8);

			//First we remove all the comments (From hashtag to newline. TODO: Does this even work on Linux/MAC?
			content = content.replaceAll("#.*(\\r\\n|\\n|\\r)", "");
			
			//Replace all §X[...] with the argument X
			for(int i=0; i<args.length; i++) {
				content = content.replaceAll("@"+(i+1)+"\\s*\\[([^\\]]*)\\]", args[i]);
			}
			
			// Replace any §X[...] that were not repalce in the previous step with '...'
			content = content.replaceAll("@(\\d+)\\s*\\[([^\\]]*)\\]", "$2");
			
			//This regex splits the input at whitespaces, only if they are followed by an even number of quotation marks. See: https://stackoverflow.com/questions/25477562/split-java-string-by-space-and-not-by-double-quotation-that-includes-space
			String[] tokens = content.split("\"?(\\s+|$)(?=(([^\"]*\"){2})*[^\"]*$)\"?");

			//Legacy version should the one above lead to problems. Quotation marks are not accounted for.
			//String[] tokens = content.split("\\s+");

			//For reasons unbeknownst to me, the parser sometimes add an empty string at the start of the list.
			while(tokens[0].isEmpty()) tokens = (String[]) Arrays.copyOfRange(tokens, 1, tokens.length);

			//Tokenizing completed. Proceed with actual parsing:
			parseDepth("", 0, tokens);

		} catch (IOException e) {
			e.printStackTrace();
		}
		catch (IndexOutOfBoundsException e) {
			error("Incorrect Token Syntax. OutOfBounds");
		}
	}

	private static int parseDepth(String context, int i, String[] content) throws IndexOutOfBoundsException, IOException {
		while(i<content.length) {
			if(content[i].equals("}")) return i+1;
			else if(content[i].equals("$embededCSV") && content[i+1].equals("=")) {
				parseEmbededCSV(context, content[i+2]);
				i+=3;
			}
			else if(content[i].equals("$embededJSON") && content[i+1].equals("=")) {
				parseEmbededJSON(context, content[i+2]);
				i+=3;
			}
			else if(is_identifier(content[i]) && content[i+1].equals("=") && content[i+2].equals("{")) {
				i = parseDepth(context+"/"+content[i], i+3, content);
			}
			else if(is_identifier(content[i]) && content[i+1].equals("=")) { //We dont care if content[i+2] is an identifier, because quotation-strings are also values.
				ForageSettings.add(context+"/"+content[i], content[i+2]);
				i += 3;
			}
			else {
				error("Incorrect Token Syntax at: " + content[i] + " " + content[i+1] + " " + content[i+2]);
				return Integer.MAX_VALUE;
			}
		}
		return i;
	}

	private static void parseEmbededJSON(String context, String path) {
		try {
			String jsonContent="";
			BufferedReader reader = new BufferedReader(new FileReader(path));

			String line;
			while ((line = reader.readLine()) != null) {
				jsonContent += " " + line;
			}
			reader.close();

			JSONObject obj = new JSONObject(jsonContent);
			parseJSONObject(context, obj);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void parseJSONObject(String context, JSONObject obj) {
		for(Iterator<String> it = obj.keys(); it.hasNext();) {
			String key = it.next();

			if(obj.get(key) instanceof JSONArray) parseJSONArray(context+"/"+key, obj.getJSONArray(key));
			else if(obj.get(key) instanceof JSONObject) parseJSONObject(context+"/"+key, obj.getJSONObject(key));
			else ForageSettings.add(context+"/"+key, obj.get(key).toString());
		}
	}

	private static void parseJSONArray(String context, JSONArray arr) {
		for (int i = 0; i < arr.length(); i++) {
			if(arr.get(i) instanceof JSONArray) parseJSONArray(context+"/"+i, arr.getJSONArray(i));
			else parseJSONObject(context+"/"+i, arr.getJSONObject(i));
		}
	}

	private static void parseEmbededCSV(String context, String path) throws IOException {
		BufferedReader reader = new BufferedReader(new FileReader(path));
		List<String[]> data = new ArrayList<>();
		String line;

		while ((line = reader.readLine()) != null) {
		    String[] dataLine = line.split(";");
			for(int i=0; i<dataLine.length; i++) {
				// Remove quotation marks from entries
				if (dataLine[i].length() >= 2 && dataLine[i].charAt(0) == '"' && dataLine[i].charAt(dataLine[i].length() - 1) == '"') dataLine[i] = dataLine[i].substring(1, dataLine[i].length() - 1);
			}
		    data.add(dataLine);
		}
		reader.close();

		// Add entries into ForageSettings as context/row/title : value.    Example: /game/rounds/0/soloRound : true
		for(int row=1; row<data.size(); row++) {
			for(int clm=0; clm<data.get(row).length; clm++) {
				ForageSettings.add(context + "/" + (row-1) + "/" + data.get(0)[clm], data.get(row)[clm]);
			}
		}
	}

	private static void error(String msg) {
		System.out.println("ERR: " + msg);
	}

	private static boolean is_identifier(String str)
	{
	    return str.matches("\\w+");
	}

	static String readFile(String path, Charset encoding) throws IOException
	{
	  byte[] encoded = Files.readAllBytes(Paths.get(path));
	  return new String(encoded, encoding);
	}
}
