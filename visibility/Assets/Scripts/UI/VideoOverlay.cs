using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Video;
using UnityEngine.UI;

public class VideoOverlay : MonoBehaviour
{
    public VideoPlayer videoPlayer;
    public RawImage rawImage;

    // Start is called before the first frame update
    void Start()
    {
        //StartCoroutine(CommenceVideo());
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    IEnumerator CommenceVideo() {
        videoPlayer.Prepare();
        while(!videoPlayer.isPrepared) {
            yield return new WaitForSeconds(0.0001f);
        }
        rawImage.texture = videoPlayer.texture;
        videoPlayer.Play();
    }
}
