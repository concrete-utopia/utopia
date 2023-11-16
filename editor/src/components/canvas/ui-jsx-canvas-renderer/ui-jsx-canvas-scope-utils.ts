import type { MapLike } from 'typescript'
import type { ArbitraryJSBlock } from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'

export function runBlockUpdatingScope(
  filePath: string,
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
): MapLike<any> {
  const result = resolveParamsAndRunJsCode(filePath, block, requireResult, currentScope)
  const definedWithinWithValues: MapLike<any> = {}
  for (const within of block.definedWithin) {
    currentScope[within] = result[within]
    definedWithinWithValues[within] = result[within]
  }
  return definedWithinWithValues
}
