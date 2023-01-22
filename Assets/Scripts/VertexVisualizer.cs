using UnityEditor;
using UnityEngine;

[ExecuteInEditMode]
public class VertexVisualizer : MonoBehaviour
{
    [SerializeField]
    private Options option;

    private Mesh mesh = null;
    private Vector3[] vertices = null;

    private enum Options
    {
        Local,
        Global,
        Viewspace,
        None
    }

    void OnDrawGizmos()
    {
        if (vertices == null)
        {
            mesh = GetComponent<MeshFilter>().sharedMesh;
            vertices = mesh.vertices;
        }

        switch (option)
        {
            case Options.Local:
                foreach (Vector3 v in vertices)
                {
                    string localVert = $"v: {v.ToString()}";
                    UnityEditor.Handles.Label(transform.position + v, localVert);
                }
                break;

            case Options.Global:
                foreach (Vector3 v in vertices)
                {
                    string worldVert = $"wv: {(transform.position + v).ToString()}";
                    UnityEditor.Handles.Label(transform.position + v, worldVert);
                }
                break;

            case Options.Viewspace:
                foreach (Vector3 v in vertices)
                {
                    Vector3 viewVector = SceneView.GetAllSceneCameras()[0].WorldToViewportPoint(transform.position + v);
                    string viewVert = $"vv: {viewVector.ToString()}";
                    UnityEditor.Handles.Label(transform.position + v, viewVert);
                }
                break;

            default:
                break;
        }
    }
}
