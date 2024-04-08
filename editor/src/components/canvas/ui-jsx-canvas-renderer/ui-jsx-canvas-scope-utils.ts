import type { MapLike } from 'typescript'
import {
  arbitraryBlockRanToEnd,
  type ArbitraryBlockResult,
  type ArbitraryJSBlock,
} from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'

export function runBlockUpdatingScope(
  filePath: string,
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
): ArbitraryBlockResult {
  const result: ArbitraryBlockResult = resolveParamsAndRunJsCode(
    filePath,
    block,
    requireResult,
    currentScope,
  )
  if (result.type === 'ARBITRARY_BLOCK_RAN_TO_END') {
    const definedWithinWithValues: MapLike<any> = {}
    for (const within of block.definedWithin) {
      currentScope[within] = result.scope[within]
      definedWithinWithValues[within] = result.scope[within]
    }
    return arbitraryBlockRanToEnd(definedWithinWithValues)
  } else {
    return result
  }
}
