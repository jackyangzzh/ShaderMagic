using UnityEngine;

[ExecuteInEditMode]
public class VertexVisualizer : MonoBehaviour
{
    [SerializeField]
    private bool showLocal = true;

    private Mesh mesh = null;
    private Vector3[] vertices = null;

    void OnDrawGizmos()
    {
        if (vertices == null)
        {
            mesh = GetComponent<MeshFilter>().sharedMesh;
            vertices = mesh.vertices;
        }

        if (showLocal)
        {
            foreach (Vector3 v in vertices)
            {
                string localVert = $"v: {v.ToString()}";
                UnityEditor.Handles.Label(transform.position + v, localVert);
            }
        }
        else
        {
            foreach (Vector3 v in vertices)
            {
                string worldVert = $"wv: {(transform.position + v).ToString()}";
                UnityEditor.Handles.Label(transform.position + v, worldVert);
            }
        }

    }
}
