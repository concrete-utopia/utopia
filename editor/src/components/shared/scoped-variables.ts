import type { ElementPath } from '../../core/shared/project-file-types'
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
  jsxFragmentWithoutUID,
  type JSExpressionOtherJavaScript,
  jsxTextBlock,
} from '../../core/shared/element-template'
import { type VariablesInScope } from '../canvas/ui-jsx-canvas'
import { toComponentId } from '../../core/shared/element-path'
import {
  type InsertableComponentGroup,
  insertableComponent,
  insertableComponentGroupProjectComponent,
} from './project-components'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { isImage } from '../../core/shared/utils'
import { type ComponentElementToInsert } from '../custom-code/code-file'

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
): AllVariablesInScope {
  const elementPathString = toComponentId(elementPath)
  const jsxComponent = topLevelElements.find((el) => isUtopiaJSXComponent(el)) as UtopiaJSXComponent
  const jsxComponentVariables = variablesInScopeFromEditorState[elementPathString] ?? {}
  return {
    filePath: jsxComponent?.name ?? 'Component',
    variables: Object.entries(jsxComponentVariables).flatMap(([name, value]) => {
      const type = getTypeByValue(value)
      if (type === 'object' && value != null) {
        // iterate only first-level keys of object
        return Object.entries(value).map(([key, innerValue]) => ({
          name: `${name}.${key}`,
          value: innerValue,
          type: getTypeByValue(innerValue),
        }))
      } else {
        return {
          name,
          value,
          type,
        }
      }
    }),
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
          { variableType: variable.type },
        )
      }),
    }
  })
}

function getMatchingElementForVariable(variable: Variable): ComponentElementToInsert {
  return getMatchingElementForVariableInner(variable).component
}

function getMatchingElementForVariableInner(variable: Variable): InsertableComponentAndJSX {
  switch (variable.type) {
    case 'string':
      return simpleInsertableComponentAndJsx('span', variable.name)
    case 'number':
      return simpleInsertableComponentAndJsx('span', variable.name)
    case 'boolean':
      return insertableComponentAndJSX(
        jsxConditionalExpressionWithoutUID(
          jsIdentifierName(variable.name),
          variable.name,
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          emptyComments,
        ),
        `${variable.name} ? null : null`,
      )
    case 'object':
      return simpleInsertableComponentAndJsx(
        'span',
        `JSON.stringify(${variable.name})`,
        variable.name,
      )
    case 'array':
      return arrayInsertableComponentAndJsx(variable)
    case 'image':
      return insertableComponentAndJSX(
        jsxElementWithoutUID(
          'img',
          jsxAttributesFromMap({
            src: jsIdentifierName(variable.name),
          }),
          [],
        ),
        `<img src={${variable.name}} style={{width: 100, height: 100, contain: 'layout'}}/>`,
      )
    default:
      return simpleInsertableComponentAndJsx(
        'span',
        `JSON.stringify(${variable.name})`,
        variable.name,
      )
  }
}

function getTypeByValue(value: unknown): InsertableType {
  const type = typeof value
  if (type === 'object' && Array.isArray(value)) {
    return 'array'
  } else if (typeof value === 'string' && isImage(value)) {
    return 'image'
  }
  return type
}

function arrayInsertableComponentAndJsx(variable: Variable): InsertableComponentAndJSX {
  const arrayElementsType = getArrayType(variable)
  const innerElementString = getMatchingElementForVariableInner({
    name: 'item',
    type: arrayElementsType,
  }).jsx
  const arrayElementString = `${variable.name}.map((item) => (${innerElementString}))`
  const arrayElement = jsxFragmentWithoutUID([jsxTextBlock(`{${arrayElementString}}`)], true)
  return insertableComponentAndJSX(arrayElement, arrayElementString)
}

function jsIdentifierName(name: string): JSExpressionOtherJavaScript {
  return jsExpressionOtherJavaScriptSimple(name, [name])
}

function getArrayType(arrayVariable: Variable): InsertableType {
  const arr = arrayVariable.value as unknown[]
  const types = new Set(arr.map((item) => getTypeByValue(item)))
  return types.size === 1 ? [...types][0] : 'object'
}

function insertableComponentAndJSX(
  component: ComponentElementToInsert,
  jsx: string,
): InsertableComponentAndJSX {
  return {
    component,
    jsx,
  }
}

function simpleInsertableComponentAndJsx(
  tag: string,
  expression: string,
  variableName: string = expression,
): InsertableComponentAndJSX {
  return insertableComponentAndJSX(
    jsxElementWithoutUID(tag, jsxAttributesFromMap({}), [
      jsExpressionOtherJavaScriptSimple(expression, [variableName]),
    ]),
    `<${tag}>{${expression}}</${tag}>`,
  )
}

interface InsertableComponentAndJSX {
  component: ComponentElementToInsert
  jsx: string
}

export type AllVariablesInScope = {
  filePath: string
  variables: Variable[]
}

type InsertableType =
  | 'string'
  | 'number'
  | 'bigint'
  | 'boolean'
  | 'symbol'
  | 'undefined'
  | 'object'
  | 'function'
  | 'array'
  | 'image'

interface Variable {
  name: string
  type: InsertableType
  value?: unknown
}
