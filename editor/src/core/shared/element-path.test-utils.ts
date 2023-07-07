import type {
  ElementPathPart,
  StaticElementPathPart,
  StaticElementPath,
} from './project-file-types'
import * as EP from './element-path'

export function testStaticElementPath(elementPaths: ElementPathPart[]): StaticElementPath {
  return EP.elementPath(elementPaths as any as StaticElementPathPart[])
}
