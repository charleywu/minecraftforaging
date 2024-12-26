using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class StayUpright : MonoBehaviour
{
    void LateUpdate()
    {
        // These two lines are meant to counteract innacuries that occur over time due to floating point
        transform.localPosition = Vector3.zero;
        transform.localRotation = Quaternion.identity;


        // Force x-rotation to 0 to maintain object upright.
        Vector3 rot = transform.rotation.eulerAngles;
        transform.rotation = Quaternion.Euler(0, rot.y, rot.z);
    }
}
