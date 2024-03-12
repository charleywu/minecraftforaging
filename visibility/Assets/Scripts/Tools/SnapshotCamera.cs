using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(Camera))]
public class SnapshotCamera : MonoBehaviour
{
    private Camera cam;
    private Texture2D prev_snapshot;

    void Start()
    {
        cam = GetComponent<Camera>();
        cam.enabled = false;
    }

    public Camera get_camera() {
        return cam;
    }

    public Texture2D TakeSnapshot() {
        // Avoiding memory leaks
        if (prev_snapshot) Object.Destroy(prev_snapshot);


        cam.enabled = true;
        //Create texture 2D
        Texture2D snapshot = new Texture2D(cam.targetTexture.width, cam.targetTexture.height, TextureFormat.RGB24, false);
        //Render camera to its targetTexture
        cam.Render();
        //Set the currently active renderTe4xture as the cam.targetTexture
        RenderTexture.active = cam.targetTexture;
        //Save screen pixels into the texture2D
        snapshot.ReadPixels(new Rect(0, 0, cam.targetTexture.width, cam.targetTexture.height), 0, 0, false);
        //Save as file (not required, just use snapshot.GetPixel)
        //byte[] bytes = snapshot.EncodeToPNG();
        //System.IO.File.WriteAllBytes("asdfgh.png", bytes);
        //Disable camera
        cam.enabled = false;

        prev_snapshot = snapshot;

        return snapshot;
    }
}
