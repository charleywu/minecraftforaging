using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;

public class Field : MonoBehaviour
{
    public GameObject BlockPrefab;
    public GameObject DirtPrefab;
    public GameObject SplashPrefab;

    GameObject[,] block;
    GameObject[,] ground;
    GameObject[,] splash;

    void Start() {
        block = new GameObject[20, 20];
        splash = new GameObject[20, 20];
        ground = new GameObject[62, 62];

        for (int i = 0; i < 62; i += 1)
        {
            for (int j = 0; j < 62; j += 1)
            {
                //Ground
                GameObject o = Instantiate(DirtPrefab, new Vector3(i, 0, j), Quaternion.identity);
                ground[i,j] = o;
            }
        }

        for (int i = 2; i < 62; i += 3)
        {
            for (int j = 2; j < 62; j += 3)
            {
                //Blocks
                GameObject o = Instantiate(BlockPrefab, new Vector3(i, 0, j), Quaternion.identity);
                block[field_to_TT(i),field_to_TT(j)] = o;

                //Splashes
                o = Instantiate(SplashPrefab, new Vector3(i, 0, j), Quaternion.identity);
                splash[field_to_TT(i), field_to_TT(j)] = o;
                foreach (ColorEncodedRenderer CER in o.GetComponentsInChildren<ColorEncodedRenderer>()) CER.init();
                o.SetActive(false);
            }
        }
    }

    public void Reset() {
        foreach (GameObject obj in block) Destroy(obj);
        foreach (GameObject obj in splash) Destroy(obj);
        foreach (GameObject obj in ground) Destroy(obj);

        Start();

        foreach (GameObject obj in block) obj.SetActive(true);
        foreach (GameObject obj in splash) obj.SetActive(false);
        foreach (GameObject obj in ground) obj.SetActive(true);
    }

    public void ApplyGameState(GameState state) {
        foreach(BlockBreakEvent e in state.block_events) {
            block[e.TTxPos, e.TTyPos].SetActive(false);
            if(e.reward) {
                //NOTE: All these "GetComponentsInChildren" is expensive and sub-optimal.
                //Splash
                splash[e.TTxPos, e.TTyPos].SetActive(true);
                foreach (ColorEncodedRenderer CER in splash[e.TTxPos, e.TTyPos].GetComponentsInChildren<ColorEncodedRenderer>()) CER.set_trigger_player(e.PName);
                splash[e.TTxPos, e.TTyPos].GetComponent<Splash>().zero_tick();

                // Place the splash on the player position:
                PlayerState triggerPlayer = state.GetPlayerStateList().Where((s) => s.PlayerName == e.PName).First();
                UnityEngine.Assertions.Assert.IsTrue(triggerPlayer != null);
                splash[e.TTxPos, e.TTyPos].transform.position = triggerPlayer.GroundPos;
            }
        }
    }

    public static int TT_to_field(int c) {
        return 1+c*3;
    }

    public static int field_to_TT(int c) {
        return (c-1)/3;
    }
}
