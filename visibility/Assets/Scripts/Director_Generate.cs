using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

public class Director_Generate : MonoBehaviour
{
    public bool running = false;
    public bool finished = false;

    public bool realTime = false;

    private float stime;

    public void commence()
    {
        Root.Instance.print("Generation Commenced.");
        ColorEncodedRenderer.static_init();
        Root.Instance.initialize_history();
        FileWriter.init();
        running = true;
        stime = Time.realtimeSinceStartup;
    }

    public void end()
    {
        Root.Instance.success("Ending Generation. (Time: " + (Time.realtimeSinceStartup - stime) + "s)");
        running = false;
        finished = true;
        FileWriter.deinit();
        Root.Instance.field.Reset();
    }

    void Start()
    {
    }

    void Update()
    {
        if (!running || finished) return;

        if(realTime && Time.realtimeSinceStartup < stime) {
            return;
        }
        else if(realTime) {
            stime += Root.Instance.deltaTime;
        }

        if (!Root.Instance.tick())
        {
            end();
        }
        else {
            Root.Instance.set_completion(Root.Instance.current().t / 120.0f);
            SaveGameStateToFile();
        }
    }

    void SaveGameStateToFile()
    {
        Texture2D rend1 = Root.Instance.p_cameras[0].TakeSnapshot();
        Texture2D rend2 = Root.Instance.p_cameras[1].TakeSnapshot();
        Texture2D rend3 = Root.Instance.p_cameras[2].TakeSnapshot();
        Texture2D rend4 = Root.Instance.p_cameras[3].TakeSnapshot();

        // We can't afford to write to file on realtime
        if (realTime) return;

        Dictionary<ColorEncodedRenderer, int> pixel_dict1 = get_pixel_dict(rend1);
        Dictionary<ColorEncodedRenderer, int> pixel_dict2 = get_pixel_dict(rend2);
        Dictionary<ColorEncodedRenderer, int> pixel_dict3 = get_pixel_dict(rend3);
        Dictionary<ColorEncodedRenderer, int> pixel_dict4 = get_pixel_dict(rend4);

        GameState curr = Root.Instance.current();

        FileWriter.Write(pixel_dict1, curr.p1);
        FileWriter.Write(pixel_dict2, curr.p2);
        FileWriter.Write(pixel_dict3, curr.p3);
        FileWriter.Write(pixel_dict4, curr.p4);
    }

    Dictionary<ColorEncodedRenderer, int> get_pixel_dict(Texture2D rend)
    {
        Dictionary<ColorEncodedRenderer, int> pixel_dict = new Dictionary<ColorEncodedRenderer, int>();

        //Count pixel groups
        for (int x = 0; x < rend.width; x++)
        {
            for (int y = 0; y < rend.height; y++)
            {
                Color c = rend.GetPixel(x, y);

                int oid = ColorEncodedRenderer.get_id_from_color(c);

                try {
                    ColorEncodedRenderer cer = ColorEncodedRenderer.CERs[oid];
                    if (pixel_dict.ContainsKey(cer)) { pixel_dict[cer]++; }
                    else { pixel_dict.Add(cer, 1); }
                }
                catch(System.IndexOutOfRangeException) {
                    Root.Instance.warn("Index out of bounds: " + oid);
                }
            }
        }
        return pixel_dict;
    }
}
