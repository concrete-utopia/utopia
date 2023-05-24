import { ContentAffectingType } from '../group-like-helpers'

export type EditorContract = 'fragment' | 'frame'

export function getEditorContractForContentAffectingType(
  type: ContentAffectingType | null,
): EditorContract {
  if (type === 'fragment') {
    return 'fragment'
  }
  return 'frame'
}
