import Utils from '../../../../utils/utils'
import { TemplatePath } from '../../../../core/shared/project-file-types'
import { OriginalPath } from '../../../editor/store/editor-state'
import * as TP from '../../../../core/shared/template-path'

export function getOriginalTemplatePath(
  originalTemplatePaths: Array<OriginalPath>,
  currentTemplatePath: TemplatePath,
): TemplatePath {
  return Utils.forceNotNull(
    `couldn't find original template path for ${TP.toComponentId(currentTemplatePath)}`,
    originalTemplatePaths.find((tpMapping) =>
      TP.pathsEqual(tpMapping.currentTP, currentTemplatePath),
    ),
  ).originalTP
}
