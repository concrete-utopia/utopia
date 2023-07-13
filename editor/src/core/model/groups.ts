import type { EditorState } from '../../components/editor/store/editor-state'
import type { ElementPath } from '../shared/project-file-types'

export function addToTrueUpGroups(
  editor: EditorState,
  ...targets: Array<ElementPath>
): EditorState {
  return {
    ...editor,
    trueUpGroupsForElementAfterDomWalkerRuns: [
      ...editor.trueUpGroupsForElementAfterDomWalkerRuns,
      ...targets,
    ],
  }
}
