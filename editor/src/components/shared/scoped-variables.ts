import type { ElementPath, VariablesInScope } from '../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../editor/store/editor-state'
import type { ProjectContentTreeRoot } from '../assets'
import {
  isArbitraryJSBlock,
  isUtopiaJSXComponent,
  type ArbitraryJSBlock,
  type UtopiaJSXComponent,
} from '../../core/shared/element-template'

export function getVariablesInScope(
  elementPath: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
): VariablesInScope[] {
  return withUnderlyingTarget(
    elementPath,
    projectContents,
    [],
    (success, element, underlyingTarget, underlyingFilePath) => {
      const topLevelCode: ArbitraryJSBlock[] = success.topLevelElements.filter((el) =>
        isArbitraryJSBlock(el),
      ) as ArbitraryJSBlock[]
      const jsxComponent = success.topLevelElements.find((el) =>
        isUtopiaJSXComponent(el),
      ) as UtopiaJSXComponent

      const topLevelVariables = topLevelCode?.map((block) => block?.definedWithin ?? []).flat()
      const componentVariables = jsxComponent?.arbitraryJSBlock?.definedWithin ?? []
      const elementName = jsxComponent?.name
      // const propsUsed = jsxComponent?.propsUsed ?? []

      return [
        {
          filePath: underlyingFilePath,
          variables: topLevelVariables.map((variable) => ({
            name: variable,
            type: 'string' as const,
          })),
        },
        {
          filePath: elementName ?? 'Component',
          variables: componentVariables.map((variable) => ({
            name: variable,
            type: 'string' as const,
          })),
        },
      ]
    },
  )
}
