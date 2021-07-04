import { MapLike } from 'typescript'
import { ArbitraryJSBlock } from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'

export function runBlockUpdatingScope(
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
  metadataContext: UiJsxCanvasContextData,
  componentPath: ElementPath | null,
): void {
  const result = resolveParamsAndRunJsCode(
    block,
    requireResult,
    currentScope,
    metadataContext,
    componentPath ?? undefined,
  )
  fastForEach(block.definedWithin, (within) => {
    currentScope[within] = result[within]
  })
}
