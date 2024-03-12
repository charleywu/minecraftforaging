using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EmitSphereParticles : MonoBehaviour
{
    private ParticleSystem sys;

    public void zero_tick() {
        sys = GetComponent<ParticleSystem>();

        List<Vector3> poslist = PseudoRandomUnitSpherePointGenerator.get_pointlist(100);

        foreach(var pos in poslist) {
            ParticleSystem.EmitParams prms = new ParticleSystem.EmitParams();
            prms.position = 2.0f * new Vector3(pos.x, Mathf.Abs(pos.y), pos.z);
            sys.Emit(prms, 3);
        }
    }

    public void tick() {
    }
}
