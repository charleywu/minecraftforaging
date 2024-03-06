package com.arc.forage.tools;

public class StyledText {
	
	String json;
	
	public StyledText() {
		json = "[";
	}
	
	public StyledText(String text, String color, String[] format) {
		json = "[";
		append(text, color, format);
	}
	
	public StyledText(String text, String color, String format) {
		json = "[";
		append(text, color, format);
	}
	
	public StyledText(String text, String color) {
		json = "[";
		append(text, color);
	}
	
	public StyledText(String text) {
		json = "[";
		append(text);
	}
	
	public String toJSON() {
		return json + "\"\"]";
	}
	
	public StyledText append(String text, String color, String format) {
		json = json + "{\"text\":\""+text+"\", \"color\":\""+color+"\",\""+format+"\":\"true\"},";
		return this;
	}
	
	public StyledText append(String text, String color, String format[]) {
		String f = "";
		for(String s : format) f = f + ", \""+s+"\":\"true\"";
		json = json + "{\"text\":\""+text+"\", \"color\":\""+color+"\""+f+"},";
		return this;
	}
	
	public StyledText append(String text, String color) {
		json = json + "{\"text\":\""+text+"\", \"color\":\""+color+"\"},";
		return this;
	}
	
	public StyledText append(String text) {
		json = json + "{\"text\":\""+text+"\"},";
		return this;
	}
}
