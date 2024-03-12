using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[ExecuteInEditMode]
public class Billboard : MonoBehaviour
{
    void OnWillRenderObject()
    {
        transform.LookAt(Camera.current.transform.position);
    }
}
