import type { MapLike } from 'typescript'
import type { ArbitraryJSBlock } from '../../../core/shared/element-template'
import type { HookResultFunction } from '../../../core/shared/javascript-cache'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'

export function runBlockUpdatingScope(
  filePath: string,
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
  setHookResult: HookResultFunction,
): void {
  const result = resolveParamsAndRunJsCode(
    filePath,
    block,
    requireResult,
    currentScope,
    setHookResult,
  )
  for (const within of block.definedWithin) {
    currentScope[within] = result[within]
  }
}
