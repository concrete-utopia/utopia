import { ElementPath, StaticElementPath, StaticTemplatePath } from './project-file-types'
import * as TP from './template-path'

export function testStaticTemplatePath(elementPaths: ElementPath[]): StaticTemplatePath {
  return TP.templatePath((elementPaths as any) as StaticElementPath[])
}
