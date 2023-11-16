import type { MapLike } from 'typescript'
import type { ArbitraryJSBlock } from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'

export function runBlockUpdatingScope(
  filePath: string,
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
): void {
  const result = resolveParamsAndRunJsCode(filePath, block, requireResult, currentScope)
  for (const within of block.definedWithin) {
    // console.log(within, result[within])
    currentScope[within] = result[within]
  }
}
