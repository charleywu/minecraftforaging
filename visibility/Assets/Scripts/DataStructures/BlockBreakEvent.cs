using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BlockBreakEvent
{
    public GameState gs;
    public string PName;
    public int xPos, yPos;
    public int TTxPos, TTyPos;
    public bool reward;

    public BlockBreakEvent(string[] l, GameState master) {
        this.gs = master;
        this.PName = l[1];
        this.xPos = (int)Helpers.ParseFloat(l[2]);
        this.yPos = (int)Helpers.ParseFloat(l[3]);
        this.TTxPos = Field.field_to_TT(xPos);
        this.TTyPos = Field.field_to_TT(yPos);
        this.reward = bool.Parse(l[4]);
    }

}
