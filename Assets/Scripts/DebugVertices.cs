using UnityEngine;

[ExecuteInEditMode]
public class DebugVertices : MonoBehaviour
{
    private Mesh mesh = null;
    private Vector3[] vertices = null;

    void OnDrawGizmos()
    {
        if (vertices == null)
        {
            mesh = GetComponent<MeshFilter>().sharedMesh;
            vertices = mesh.vertices;
        }

        foreach (Vector3 v in vertices)
        {
            UnityEditor.Handles.Label(transform.position + v, "v: " + v.ToString());
        }
    }
}
