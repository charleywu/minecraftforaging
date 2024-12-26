using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

public class Director_Process : MonoBehaviour
{
    private List<string> ppathsList, bpathsList;
    private int i = 0;

    private Director_Generate dir;

    public void commence() {
        i = 0;
        var ppathsFile = File.ReadAllLines("players_paths.txt");
        ppathsList = new List<string>(ppathsFile);

        var bpathsFile = File.ReadAllLines("blocks_paths.txt");
        bpathsList = new List<string>(bpathsFile);

        init(i);
    }

    void init(int index) {
        if(index >= bpathsList.Count) {
            Root.Instance.success("PROCESS COMPLETED.");
            return;
        }

        Root.Instance.set_blocks_file("." + bpathsList[index]);
        Root.Instance.set_player_file("." + ppathsList[index]);

        dir = gameObject.AddComponent(typeof(Director_Generate)) as Director_Generate;
        dir.commence();
    }


    void Update()
    {
        if(dir!=null && dir.finished) {
            Destroy(GetComponent(typeof(Director_Generate)));
            i++;
            init(i);
        }
    }
}
