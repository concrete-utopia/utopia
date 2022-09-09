import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'

export function isReorderAllowed(siblings: Array<ElementPath>) {
  return siblings.every((sibling) => !isRootOfGeneratedElement(sibling))
}

function isRootOfGeneratedElement(target: ElementPath): boolean {
  const uid = EP.toUid(target)
  const staticUid = EP.toStaticUid(target)
  return uid !== staticUid
}
