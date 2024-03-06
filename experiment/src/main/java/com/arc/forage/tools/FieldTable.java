package com.arc.forage.tools;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class FieldTable {
	public int row_count = 0;
	public int clm_count = 0;
	public boolean initialized = false;
	
	private List<String[]> data;
	
	public FieldTable(String path) {
		try {
			parse(path);
		}
		catch (IOException e) {
			System.out.println(">> ERR: " + path + "COULD NO BE FOUND!");
			e.printStackTrace();
		}
	}
	
	public boolean get(int x, int y) {
		try {
			return data.get(y+1)[x+1].equals("1"); //+1 because first row/column are csv headers
		}
		catch(IndexOutOfBoundsException e) {
			System.out.println("Attempted to access fieltable field out of bounds! x:"+(x+1)+" y:"+(y+1)+" size:"+row_count+"x"+clm_count);
			return false;
		}
	}
	
	private void parse(String path) throws IOException {
		BufferedReader reader = new BufferedReader(new FileReader(path));
		data = new ArrayList<>();
		String line;
		
		while ((line = reader.readLine()) != null) {
		    String[] dataLine = line.split(";");
			for(int i=0; i<dataLine.length; i++) {
				// Remove quotation marks from entries
				if (dataLine[i].length() >= 2 && dataLine[i].charAt(0) == '"' && dataLine[i].charAt(dataLine[i].length() - 1) == '"') dataLine[i] = dataLine[i].substring(1, dataLine[i].length() - 1);
			}
		    data.add(dataLine);
		}
		row_count = data.size()-1;		//Subtract headers
		clm_count = data.get(1).length-1;
		
		initialized = check_valid();
		
		reader.close();
	}
	
	private boolean check_valid() {
		if(row_count<0 || row_count+1 != data.size()) return false;
		for(int i=1; i<data.size(); i++) { //Skip header
			if(clm_count<0 || clm_count+1 != data.get(i).length) return false;
		}
		return true;
	}
}
