import { isProjectViewerFromState } from './project-server-state'
import { Substores, useEditorState } from './store-hook'

export function useIsViewer(): boolean {
  const isViewer = useEditorState(
    Substores.projectServerState,
    (store) => isProjectViewerFromState(store.projectServerState),
    'useIsViewer isViewer',
  )
  return isViewer
}
