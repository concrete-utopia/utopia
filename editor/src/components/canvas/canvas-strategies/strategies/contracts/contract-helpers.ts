import { ContentAffectingType } from '../group-like-helpers'

export type EditorContract = 'fragment' | 'frame' | 'not-quite-frame'

export function getEditorContractForContentAffectingType(
  type: ContentAffectingType | null,
): EditorContract {
  if (type === 'fragment' || type === 'conditional') {
    return 'fragment'
  }
  if (type === 'sizeless-div') {
    return 'not-quite-frame'
  }
  return 'frame'
}
