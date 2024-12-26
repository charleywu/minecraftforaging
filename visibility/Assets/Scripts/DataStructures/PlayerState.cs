using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerState
{
    public GameState gs;

    public string PlayerName;
    public Vector3 Pos;
    public Vector3 ViewVector;
    public List<string> vis;
    public string vis_simple;

    public PlayerState(string[] l, GameState master) {
        this.gs = master;
        this.PlayerName = l[1];

        this.Pos = new Vector3(Helpers.ParseFloat(l[2]), 1.62f, Helpers.ParseFloat(l[3])); //1.62 = minecraft player height (camera)
        this.ViewVector = new Vector3(Helpers.ParseFloat(l[4]), Helpers.ParseFloat(l[5]), Helpers.ParseFloat(l[6]));

        string v = l[7];
        if(v.Length>2) {
            this.vis_simple = l[7];
            string[] vt = v.Substring(1, v.Length-2).Split(',');
            this.vis = new List<string>(vt);
        }
        else {
            this.vis_simple = "[]";
            this.vis = new List<string>();
        }
    }

    public Vector3 GroundPos => Pos - new Vector3(0, 1.62f, 0);


}
