using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Root : MonoBehaviour
{
    public Text output;
    public float deltaTime = 0.05f;
    public int CER_id_multiplier = 1;
    public LoadingBar completion;

    // Option to disable initializing CERs for purposes of visualization and testing
    public bool init_CERs = true;

    // Material used for regular, unlit objects
    public Material UnlitMaterial;
    //Material used for particle objects
    public Material UnlitMaterial_Particle;

    // List of player-transforms and camera
    public Transform[] p_transforms = new Transform[4]; //Transforms of player objects
    public SnapshotCamera[] p_cameras = new SnapshotCamera[4]; //Cameras of player objects

    // Reference to field
    public Field field;

    // Lists of active objects that are ticked each tick
    public List<Splash> active_splashes = new List<Splash>();

    // History of events transpiring
    private List<GameState> history;
    private int history_index = -1;

    public static Root Instance { get; private set; }

    public string players_file="test.csv", blocks_file="test_blocks.csv";

    void Awake()
    {
        if (Instance != null) GameObject.Destroy(Instance);
        else Instance = this;

        DontDestroyOnLoad(this);

        Time.timeScale = 0.0f;
    }

    void Start() {
        print("Demo-Detlatime: " + deltaTime);
        print("CER_id_multiplier: " + CER_id_multiplier);
        print("init_CERs: " + init_CERs);
        print("x_RenderResolution: " + get_x_RenderResolution());
        print("y_RenderResolution: " + get_y_RenderResolution());
    }

    public void print(string s)
    {
        if(output) output.text += "\n> " + s;
    }

    public void success(string s)
    {
        if(output) output.text += "\n# <color=green><b>" + s + "</b></color>";
    }

    public void error(string s)
    {
        if(output) output.text += "\n! <color=red><b>ERR:</b> " + s + "</color>";
    }

    public void warn(string s)
    {
        if(output) output.text += "\n! <color=yellow><b>WRN:</b> " + s + "</color>";
    }

    public void set_completion(float v) {
        completion.set_value(v);
    }

    public int get_x_RenderResolution() {
        return p_cameras[0] ? p_cameras[0].get_camera().targetTexture.width : 0;
    }

    public int get_y_RenderResolution()
    {
        return p_cameras[0] ? p_cameras[0].get_camera().targetTexture.height : 0;
    }

    public void set_player_file(string f) {
        print("New Player file: " + f);
        players_file = f;
    }

    public void set_blocks_file(string f) {
        print("New Blocks file: " + f);
        blocks_file = f;
    }

    public void initialize_history() {
        active_splashes.Clear();
        history_index = -1;
        history = CSVReader.Read(players_file, blocks_file);
        //Apply player tags:
        p_transforms[0].gameObject.tag = history[0].p1.PlayerName;
        p_transforms[1].gameObject.tag = history[0].p2.PlayerName;
        p_transforms[2].gameObject.tag = history[0].p3.PlayerName;
        p_transforms[3].gameObject.tag = history[0].p4.PlayerName;
    }

    public GameState current() {
        return history[history_index];
    }

    public bool tick() {
        history_index++;
        if(history_index>=history.Count) return false;

        // Set player positions/rotations
        ApplyPlayersToGameState(history[history_index]);

        // Tick all splash effects
        for (int i = active_splashes.Count - 1; i >= 0; i--)
        {
            active_splashes[i].tick();
        }

        // Apply BlockBreakEvents to Field
        field.ApplyGameState(history[history_index]);

        return true;
    }

    void ApplyPlayersToGameState(GameState state) {
        p_transforms[0].position = state.p1.Pos;
        p_transforms[0].LookAt(state.p1.Pos + state.p1.ViewVector);

        p_transforms[1].position = state.p2.Pos;
        p_transforms[1].LookAt(state.p2.Pos + state.p2.ViewVector);

        p_transforms[2].position = state.p3.Pos;
        p_transforms[2].LookAt(state.p3.Pos + state.p3.ViewVector);

        p_transforms[3].position = state.p4.Pos;
        p_transforms[3].LookAt(state.p4.Pos + state.p4.ViewVector);
    }
}
