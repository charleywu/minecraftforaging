using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CSVReader
{
    public static List<GameState> Read(string player_file, string blocks_file) {
        List<GameState> ret = new List<GameState>();

        string[][] data = from_csv(player_file);
        string[][] block_data = from_csv(blocks_file);

        int j = 0;
        for(int i=0; i+3<data.Length; i+=4) { //4 is player amount.

            List<string[]> blockevents = new List<string[]>();
            while(j<block_data.Length && Helpers.ParseFloat(block_data[j][0]) <= Helpers.ParseFloat(data[i][0])) {
                blockevents.Add(block_data[j]);
                j++;
            }

            ret.Add(new GameState(data[i], data[i+1], data[i+2], data[i+3], blockevents));
        }
        return ret;
    }

    private static string[][] from_csv(string path) {
        string textdata = System.IO.File.ReadAllText(path);
        string[] lines = textdata.Split("\n"[0]);
        List<string[]> data_l = new List<string[]>();

        foreach (string s in lines)
        {
            if(s.Length>1) data_l.Add((s.Trim()).Split(";"[0]));
        }

        data_l.RemoveAt(0); //Remove headers

        string[][] data = data_l.ToArray();
        return data;
    }
}
