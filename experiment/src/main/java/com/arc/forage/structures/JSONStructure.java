package com.arc.forage.structures;

import java.io.Serializable;

public class JSONStructure implements Serializable {
	private static final long serialVersionUID = 1L;
	public int pos_x, pos_y, pos_z;
	public int siz_x, siz_y, siz_z;
	
	public JSONStructure(int pos_x, int pos_y, int pos_z, int siz_x, int siz_y, int siz_z) {
		this.pos_x = pos_x;
		this.pos_y = pos_y;
		this.pos_z = pos_z;
		this.siz_x = siz_x;
		this.siz_y = siz_y;
		this.siz_z = siz_z;
	}
	
	@Override
	public String toString() {
		return "Pos_x:" + pos_x + "\nPos_y: " + pos_y + "\nPos_z: " + pos_z + "\nSiz_x:" + siz_x + "\nSiz_y: " + siz_y + "\nSiz_z: " + siz_z;
	}
}