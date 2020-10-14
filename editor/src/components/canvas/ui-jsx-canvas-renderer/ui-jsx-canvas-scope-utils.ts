import { MapLike } from 'typescript'
import { ArbitraryJSBlock } from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import { fastForEach } from '../../../core/shared/utils'

export function runBlockUpdatingScope(
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
): void {
  const result = resolveParamsAndRunJsCode(block, requireResult, currentScope)
  fastForEach(block.definedWithin, (within) => {
    currentScope[within] = result[within]
  })
}
