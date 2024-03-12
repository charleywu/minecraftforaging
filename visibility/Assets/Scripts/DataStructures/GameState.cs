using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GameState
{
    public float t;
    public PlayerState p1;
    public PlayerState p2;
    public PlayerState p3;
    public PlayerState p4;
    public List<BlockBreakEvent> block_events; //Only this frame

    public GameState(string[] p1, string[] p2, string[] p3, string[] p4, List<string[]> block_events) {
        this.t = Helpers.ParseFloat(p1[0]);
        if(Helpers.ParseFloat(p2[0])!=t || Helpers.ParseFloat(p3[0]) != t || Helpers.ParseFloat(p4[0]) != t) {
            Root.Instance.error("Something went wrong with parsing!");
        }

        this.p1 = new PlayerState(p1, this);
        this.p2 = new PlayerState(p2, this);
        this.p3 = new PlayerState(p3, this);
        this.p4 = new PlayerState(p4, this);

        this.block_events = new List<BlockBreakEvent>();
        foreach(string[] be in block_events) {
            this.block_events.Add(new BlockBreakEvent(be, this));
        }
    }

    public List<PlayerState> GetPlayerStateList()
    {
        return new List<PlayerState> { p1, p2, p3, p4 };
    }
}
