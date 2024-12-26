using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Obj_type = ColorEncodedRenderer.Obj_type;
using System.IO;

public class FileWriter
{
    private static StreamWriter evis_sr;
    private static StreamWriter pvis_sr;

    public static void init() {
        string filename = Root.Instance.players_file;
        //filename.Replace("_players.log.csv", "_evis.log.csv");

        //evis_sr = File.CreateText("evis.txt");
        evis_sr = File.CreateText(filename.Replace("_players.log.csv", "_evis.log.csv") );
        evis_sr.WriteLine("Time;PlayerName;eventtype;eventid;xPos;yPos;zPos;Occupancy;TriggerPlayer");


        //pvis_sr = File.CreateText("pvis.txt");
        pvis_sr = File.CreateText(filename.Replace("_players.log.csv", "_pvis.log.csv"));
        pvis_sr.WriteLine("Time;PlayerName;P1;P2;P3;P4");
    }

    public static void deinit() {
        evis_sr.Close();
        pvis_sr.Close();
    }

    public static void Write(Dictionary<ColorEncodedRenderer, int> pixel_dict, PlayerState p) {
        GameState curr = Root.Instance.current();
        var pVis = new Dictionary<string, float>{ { "MPIB1", 0.0f }, { "MPIB2", 0.0f }, { "MPIB3", 0.0f }, { "MPIB4", 0.0f } };

        foreach(ColorEncodedRenderer cer in pixel_dict.Keys) {
            float occupancy = pixel_dict[cer] / (float)(Root.Instance.get_x_RenderResolution() * Root.Instance.get_y_RenderResolution());

            // Reward-Events
            if(cer.type==Obj_type.SPLASH_EFFECT) {
                string line = Helpers.F2S(curr.t) + ";" 
                    + p.PlayerName + ";"                            // Player whose perspective we are considering
                    + cer.Obj_type_s() + ";"                        // Event type
                    + cer.get_id() + ";"                            // Event id
                    + Helpers.F2S(cer.transform.position.x) + ";"   // x-Position of item/event
                    + Helpers.F2S(cer.transform.position.y) + ";"   // y-Position of item/event
                    + Helpers.F2S(cer.transform.position.z) + ";"   // z-Position of item/event
                    + Helpers.F2S(occupancy) + ";"                  // Occupancy of the item/event
                    + cer.get_trigger_player();                     // Trigger-player of the item/event
                evis_sr.WriteLine(line);
            }

            //Player visibility
            else if (cer.type==Obj_type.PLAYER) {
                pVis[cer.gameObject.tag] = occupancy;
            }
        }
        string pvis_line = Helpers.F2S(curr.t) + ";" + p.PlayerName + ";" + Helpers.F2S(pVis["MPIB1"]) + ";" + Helpers.F2S(pVis["MPIB2"]) + ";" + Helpers.F2S(pVis["MPIB3"]) + ";" + Helpers.F2S(pVis["MPIB4"]);
        pvis_sr.WriteLine(pvis_line);
    }
}
