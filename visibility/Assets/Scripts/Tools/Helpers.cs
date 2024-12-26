using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Globalization;

public class Helpers
{
    public static float ParseFloat(string s) {
        return float.Parse(s, CultureInfo.InvariantCulture.NumberFormat);
    }

    public static string F2S(float f) {
        //return f.ToString("0.00000", CultureInfo.InvariantCulture); //5 decimals
        return f.ToString(CultureInfo.InvariantCulture);
    }
}
