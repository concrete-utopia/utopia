import urlJoin from 'url-join'

export function projectEditorLink(projectId: string | null): string {
  const editorURL = window.ENV.EDITOR_URL
  if (editorURL == null) {
    throw new Error('missing editor url')
  }
  return urlJoin(editorURL, 'project', projectId ?? '')
}
