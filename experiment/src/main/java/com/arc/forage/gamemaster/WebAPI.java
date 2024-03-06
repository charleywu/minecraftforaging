package com.arc.forage.gamemaster;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

public class WebAPI {
	static String read_path="web_api/mod_read.txt";
	static String write_path="web_api/mod_write.txt";
	static private int c = 0;
	static private int step = 4;
	static Watchdog wdg;
	
	void init() {
		clear_requests();
		wdg = new Watchdog();
		wdg.start();
	}
	
	static boolean has_unprocessed_Requests() {
		if(wdg.g2g) {
			wdg.g2g = false;
			return true;
		}
		return false;
	}
	
	static void process_requests() {
		BufferedReader reader;
		try {
			reader = new BufferedReader(new FileReader(read_path));
			String line = reader.readLine();
			while (line != null) {
				GameMaster.command(line);
				line = reader.readLine();
			}
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		clear_requests();
	}
	
	static private void clear_requests() {
		try {
			FileOutputStream writer = new FileOutputStream(read_path);
			writer.write(("").getBytes());
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	protected class Watchdog extends Thread {
		
		public volatile boolean g2g = false;
		private volatile boolean cancelled;
		
		public void run() {
			while(!cancelled) {
	            try { Thread.sleep(500);} catch (InterruptedException e) { e.printStackTrace(); } // wait 250 ms
	            File file = new File(read_path);
	            g2g = file.exists() && file.length()>0;
			}
        }
		public void cancel()
		{
			cancelled = true;  
		}
	}
	
	public static void set_status(String status) {
		try {
			FileOutputStream writer = new FileOutputStream(write_path);
			writer.write(("<font color=\"lime\">"+status+"</font><br><br>").getBytes());
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void set_error(String status) {
		try {
			FileOutputStream writer = new FileOutputStream(write_path);
			writer.write(("<font color=\"red\">ERR: "+status+"</font><br><br>").getBytes());
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void add_info(String status) {
		try {
			File file = new File(write_path);
			FileWriter fr = new FileWriter(file, true);
			fr.write("<font color=\"white\">"+status+"</font><br>");
			fr.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
