import {
  ElementPath,
  StaticElementPath,
  StaticInstancePath,
  StaticScenePath,
} from './project-file-types'
import * as TP from './template-path'

export function testStaticScenePath(elementPaths: ElementPath[]): StaticScenePath {
  return TP.staticScenePath((elementPaths as any) as StaticElementPath[])
}

export function testStaticInstancePath(
  scenePath: StaticScenePath,
  instanceElementPath: ElementPath,
): StaticInstancePath {
  return TP.staticInstancePath(scenePath, (instanceElementPath as any) as StaticElementPath)
}
