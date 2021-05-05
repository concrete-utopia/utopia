import Utils from '../../../../utils/utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { OriginalPath } from '../../../editor/store/editor-state'
import * as EP from '../../../../core/shared/element-path'

export function getOriginalElementPath(
  originalElementPaths: Array<OriginalPath>,
  currentElementPath: ElementPath,
): ElementPath {
  return Utils.forceNotNull(
    `couldn't find original template path for ${EP.toComponentId(currentElementPath)}`,
    originalElementPaths.find((tpMapping) =>
      EP.pathsEqual(tpMapping.currentTP, currentElementPath),
    ),
  ).originalTP
}
