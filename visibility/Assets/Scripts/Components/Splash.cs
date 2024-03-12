using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Splash : MonoBehaviour
{

    private float t = 0.0f;

    private ParticleSystem splsh;
    private EmitSphereParticles grr;

    public void tick()
    {
        t += Root.Instance.deltaTime;
        grr.tick();
        UpdateState();
    }

    public void zero_tick()
    {
        grr = GetComponent<EmitSphereParticles>();
        splsh = GetComponent<ParticleSystem>();

        splsh.time = 0.0f;
        splsh.Pause();

        grr.zero_tick();

        if (!Root.Instance.active_splashes.Contains(this)) Root.Instance.active_splashes.Add(this);
        t = 0.0f;
    }

    void UpdateState()
    {
        splsh.Simulate(Root.Instance.deltaTime, true, false, true);

        if(t>3.0f) {
            Root.Instance.active_splashes.Remove(this);
            gameObject.SetActive(false);
        }
    }
}
