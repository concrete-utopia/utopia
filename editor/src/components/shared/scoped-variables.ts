import type {
  ElementPath,
  AllVariablesInScope,
  Variable,
} from '../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../editor/store/editor-state'
import type { ProjectContentTreeRoot } from '../assets'
import {
  isArbitraryJSBlock,
  isUtopiaJSXComponent,
  type ArbitraryJSBlock,
  type UtopiaJSXComponent,
  type TopLevelElement,
  jsxAttributesFromMap,
  jsxElementWithoutUID,
  jsExpressionValue,
  jsxConditionalExpressionWithoutUID,
  emptyComments,
  jsExpressionOtherJavaScriptSimple,
} from '../../core/shared/element-template'
import { type VariablesInScope } from '../canvas/ui-jsx-canvas'
import { toComponentId } from '../../core/shared/element-path'
import {
  type InsertableComponentGroup,
  insertableComponent,
  insertableComponentGroupProjectComponent,
} from './project-components'
import { emptyImports } from '../../core/workers/common/project-file-utils'

export function getVariablesInScope(
  elementPath: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
  variablesInScopeFromEditorState: VariablesInScope,
): AllVariablesInScope[] {
  return withUnderlyingTarget(
    elementPath,
    projectContents,
    [],
    (success, element, underlyingTarget, underlyingFilePath) => {
      let varsInScope = []

      if (elementPath !== null) {
        const componentScopedVariables = getVariablesFromComponent(
          success.topLevelElements,
          elementPath,
          variablesInScopeFromEditorState,
        )
        varsInScope.push(componentScopedVariables)
      }

      /** for future reference - adding variables from top level **/
      // const topLevelVariables = getTopLevelVariables(success.topLevelElements, underlyingFilePath)
      // varsInScope.push(topLevelVariables)

      return varsInScope
    },
  )
}

function getTopLevelVariables(topLevelElements: TopLevelElement[], underlyingFilePath: string) {
  const topLevelCode: ArbitraryJSBlock[] = topLevelElements.filter((el) =>
    isArbitraryJSBlock(el),
  ) as ArbitraryJSBlock[]
  const topLevelVariables = topLevelCode?.map((block) => block?.definedWithin ?? []).flat()

  return {
    filePath: underlyingFilePath,
    variables: topLevelVariables.map((variable) => ({
      name: variable,
      type: 'string' as const,
    })),
  }
}

function getVariablesFromComponent(
  topLevelElements: TopLevelElement[],
  elementPath: ElementPath,
  variablesInScopeFromEditorState: VariablesInScope,
) {
  const elementPathString = toComponentId(elementPath)
  const jsxComponent = topLevelElements.find((el) => isUtopiaJSXComponent(el)) as UtopiaJSXComponent
  const jsxComponentVariables = variablesInScopeFromEditorState[elementPathString] ?? {}
  return {
    filePath: jsxComponent?.name ?? 'Component',
    variables: Object.entries(jsxComponentVariables).map(([name, value]) => ({
      name,
      value,
      type: typeof value,
    })),
  }
}

export function convertVariablesToElements(
  variableInScope: AllVariablesInScope[],
): InsertableComponentGroup[] {
  return variableInScope.map((variableGroup) => {
    return {
      source: insertableComponentGroupProjectComponent(variableGroup.filePath),
      insertableComponents: variableGroup.variables.map((variable) => {
        return insertableComponent(
          emptyImports(),
          () => getMatchingElementForVariable(variable),
          variable.name,
          [],
          null,
        )
      }),
    }
  })
}

function getMatchingElementForVariable(variable: Variable) {
  switch (variable.type) {
    case 'string':
      return jsxElementWithoutUID('span', jsxAttributesFromMap({}), [
        jsExpressionOtherJavaScriptSimple(variable.name, [variable.name]),
      ])
    case 'number':
      return jsxElementWithoutUID('span', jsxAttributesFromMap({}), [
        jsExpressionOtherJavaScriptSimple(variable.name, [variable.name]),
      ])
    case 'boolean':
      return jsxConditionalExpressionWithoutUID(
        jsExpressionOtherJavaScriptSimple(variable.name, [variable.name]),
        variable.name,
        jsExpressionValue(null, emptyComments),
        jsExpressionValue(null, emptyComments),
        emptyComments,
      )
    case 'object':
      return jsxElementWithoutUID('span', jsxAttributesFromMap({}), [
        jsExpressionOtherJavaScriptSimple(`JSON.stringify(${variable.name})`, [variable.name]),
      ])
    default:
      return jsxElementWithoutUID('span', jsxAttributesFromMap({}), [
        jsExpressionOtherJavaScriptSimple(`JSON.stringify(${variable.name})`, [variable.name]),
      ])
  }
}
