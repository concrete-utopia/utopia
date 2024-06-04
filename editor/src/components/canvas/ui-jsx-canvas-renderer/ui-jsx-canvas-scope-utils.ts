import type { MapLike } from 'typescript'
import {
  arbitraryBlockRanToEnd,
  type ArbitraryBlockResult,
  type ArbitraryJSBlock,
} from '../../../core/shared/element-template'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import type { ElementPath } from '../../../core/shared/project-file-types'
import {
  fileRootPathToString,
  insertionCeilingToString,
  type FileRootPath,
  type VariableData,
} from '../ui-jsx-canvas'

export function runBlockUpdatingScope(
  elementPath: ElementPath | FileRootPath | null,
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
    const definedWithinVariableData: VariableData = {}
    for (const within of block.definedWithin) {
      currentScope[within] = result.scope[within]
      definedWithinWithValues[within] = result.scope[within]
      if (elementPath != null) {
        definedWithinVariableData[within] = {
          spiedValue: result.scope[within],
          insertionCeiling: elementPath,
        }
      }
    }
    return arbitraryBlockRanToEnd(definedWithinWithValues, definedWithinVariableData)
  } else {
    return result
  }
}
