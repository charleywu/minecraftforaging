using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;

public class ColorEncodedRenderer : MonoBehaviour
{
    // Static/Class based Components
    public static List<ColorEncodedRenderer> CERs_l = new List<ColorEncodedRenderer>();
    public static ColorEncodedRenderer[] CERs;
    public static bool static_initialized = false;
    private static int id_counter = 1;

    //Call this after all CERs are registered to allow searching in the CERs array
    public static void static_init() {
        CERs = CERs_l.ToArray();
        //Verify that ids have been set correclty:
        for(int i=0; i<CERs.Length; i++) {
            if(CERs[i].get_id()!=i) {
                Root.Instance.warn("IDs incorrectly set!");
            }
        }
        static_initialized = true;
    }



    public enum Obj_type {GROUND = 0, HORIZON=1, PLAYER=2, SPLASH_EFFECT=3, GREEN_BLOCK=4};
    private static string[] Obj_type_string = {"GROUND", "HORIZON", "PLAYER", "SPL", "MELON"};

    // Properties set in Unity inspector:
    public List<MeshRenderer> rend;
    public List<ParticleSystemRenderer> part;
    public Obj_type type;
    public bool share_id_across_type = false;

    // r-g-b values of the encoded color
    [HideInInspector]
    public byte r, g, b;
    
    // Variables for providing IDs
    private int id = -1;
    private int mult_id = -1;

    //Player who broke this reward block (in the the case of splash, firework etc.)
    private string trigger_player="";

    private bool initialized = false;

    public void init() {
        // Ascribe id:
        // ID = 0 is reserved for Horizon. Don't advance id_counter here.
        if (this.type == Obj_type.HORIZON)
        {
            this.id = 0;
            CERs_l.Insert(0, this);
        }
        // id is not to be shared or no type of this object has been registered -> register
        else if (!share_id_across_type || !CERs_l.Any(c => c.type == this.type))
        {
            this.id = id_counter++;
            CERs_l.Add(this);
        }
        // if is shared by other object -> ascribe id of other objects to this one
        else
        {
            this.id = CERs_l.First(c => c.type == this.type).get_id();
        }

        // Set Material Color based on id
        mult_id = this.id * Root.Instance.CER_id_multiplier;
        r = get_r();
        g = get_g();
        b = get_b();


        // Set materials
        Material rmat = new Material(Root.Instance.UnlitMaterial);
        Material pmat = new Material(Root.Instance.UnlitMaterial_Particle);

        rmat.SetColor("_Color", new Color32(r, g, b, 255));
        pmat.SetColor("_Color", new Color32(r, g, b, 255));

        if (Root.Instance.init_CERs)
        {
            foreach (MeshRenderer r in rend) r.material = rmat;
            foreach (ParticleSystemRenderer r in part) r.material = pmat;
        }

        initialized = true;
    }

    public int get_id() {
        return id;
    }

    void Start() {
        if (!initialized) init();
    }

    public void set_trigger_player(string pname) {
        this.trigger_player = pname;
    }

    public string get_trigger_player() {
        return this.trigger_player;
    }

    public string Obj_type_s()
    {
        return Obj_type_string[(int)this.type];
    }




    // RGB ENCODING -----------------------------
    /*
    HEX Numbers:
    RGB -> 24 bits
    first 12: transparent objects
    last 12: non-transparent objects
    */

    byte get_r() 
    {
        return (byte) (mult_id >> 16);
    }

    byte get_g()
    {
        return (byte) ((mult_id >> 8) & 255);
    }

    byte get_b()
    {
        return (byte) (mult_id & 255);
    }

    // TODO
    public static int get_id_from_color(Color c) 
    {
        int r = (int)(c.r * 255.0f);
        int g = (int)(c.g * 255.0f);
        int b = (int)(c.b * 255.0f);

        return ((r << 16) + (g << 8) + b) / Root.Instance.CER_id_multiplier;
    }

    // DICTIONARY STUFF --------------------------
    public bool Equals(ColorEncodedRenderer r)
    {
        return this.id.Equals(r.id);
    }

    public override bool Equals(object other)
    {
        return this.Equals(other as ColorEncodedRenderer);
    }

    public override int GetHashCode()
    {
        return this.id;
    }

}
