export async function useJurassicKey({
  jurassicBaseUrl,
  projectId,
  key,
  cb,
}: {
  jurassicBaseUrl: string
  projectId: string
  key: string
  cb: (_: string) => void
}) {
  const result = await fetch(`${jurassicBaseUrl}/api/${projectId}/${key}`).then((r) => r.text())
  cb(result)
}
