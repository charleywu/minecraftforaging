using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class LoadingBar : MonoBehaviour
{
    public Image fill_rect;

    public void set_value(float v) {
        fill_rect.fillAmount = v;
    }
}
