import type { ElementPath } from '../../core/shared/project-file-types'
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
) {
  return withUnderlyingTarget(
    elementPath,
    projectContents,
    [],
    (success, element, underlyingTarget, underlyingFilePath) => {
      const topLevelCode: ArbitraryJSBlock = success.topLevelElements.find((el) =>
        isArbitraryJSBlock(el),
      ) as ArbitraryJSBlock
      const jsxComponent = success.topLevelElements.find((el) =>
        isUtopiaJSXComponent(el),
      ) as UtopiaJSXComponent

      const topLevelVariables = topLevelCode?.definedWithin ?? []
      const componentVariables = jsxComponent?.arbitraryJSBlock?.definedWithin ?? []
      // const propsUsed = jsxComponent?.propsUsed ?? []
      const allVariableNames = [
        ...new Set([...topLevelVariables, ...componentVariables /*, ...propsUsed*/]),
      ]

      return [
        {
          filePath: underlyingFilePath,
          variables: allVariableNames.map((variable) => ({
            name: variable,
            type: 'string' as const,
          })),
        },
      ]
    },
  )
}
