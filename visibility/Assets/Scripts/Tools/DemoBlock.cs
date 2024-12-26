using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DemoBlock : MonoBehaviour
{
    public bool start = false;

    public Transform splash;
    public Transform green_block;
    public VideoOverlay overlay;

    private float stime;

    void Start() {
        foreach (ColorEncodedRenderer CER in splash.gameObject.GetComponentsInChildren<ColorEncodedRenderer>()) CER.init();
        stime = Time.realtimeSinceStartup;
    }
    
    void Update() {
        if(Input.GetKeyDown(KeyCode.Space)) {
            overlay.videoPlayer.Stop();
            overlay.videoPlayer.Play();
            start = true;
        }

        if(start) {
            green_block.gameObject.SetActive(false);
            splash.gameObject.SetActive(true);

            splash.gameObject.GetComponent<Splash>().zero_tick();
            start = false;

            stime = Time.realtimeSinceStartup;
        }
        while(Time.realtimeSinceStartup >= stime) {
            // Tick all splash effects
            for (int i = Root.Instance.active_splashes.Count - 1; i >= 0; i--)
            {
                Root.Instance.active_splashes[i].tick();
            }

            stime += Root.Instance.deltaTime;
        }
    }


}
