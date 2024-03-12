using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ColorId
{
    public byte r, g, b;

    public ColorId(byte r, byte g, byte b) {
        this.r = r;
        this.g = g;
        this.b = b;
    }

    public ColorId(float r, float g, float b) {
        this.r = (byte)(r * 255.0f);
        this.g = (byte)(g * 255.0f);
        this.b = (byte)(b * 255.0f);
    }

    public string to_hex() {
        return r.ToString("X2") + g.ToString("X2") + b.ToString("X2");
    }

    // DICTIONARY STUFF --------------------------
    public bool Equals(ColorId c)
    {
        return this.r==c.r && this.g==c.g && this.b==c.b;
    }

    public override bool Equals(object other)
    {
        return this.Equals(other as ColorId);
    }

    public override int GetHashCode()
    {
        return r + g*256 + b*65536;
    }
}
