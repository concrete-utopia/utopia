import type { ElementPath } from '../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../editor/store/editor-state'
import type { ProjectContentTreeRoot } from '../assets'
import type {
  ElementInstanceMetadataMap,
  ArbitraryJSBlock,
  UtopiaJSXComponent,
  TopLevelElement,
} from '../../core/shared/element-template'
import {
  isArbitraryJSBlock,
  isUtopiaJSXComponent,
  jsxAttributesFromMap,
  jsxElementWithoutUID,
  jsExpressionValue,
  jsxConditionalExpressionWithoutUID,
  emptyComments,
  jsExpressionOtherJavaScriptSimple,
  jsxFragmentWithoutUID,
  jsxTextBlock,
  isJSXElement,
  isJSXAttributesEntry,
  isJSXAttributeValue,
  simplifyAttributeIfPossible,
} from '../../core/shared/element-template'
import type { VariableData } from '../canvas/ui-jsx-canvas'
import { type VariablesInScope } from '../canvas/ui-jsx-canvas'
import { getContainingComponent, toComponentId } from '../../core/shared/element-path'
import {
  type InsertableComponentGroup,
  insertableComponent,
  insertableComponentGroupProjectComponent,
} from './project-components'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { isImage } from '../../core/shared/utils'
import { type ComponentElementToInsert } from '../custom-code/code-file'
import { omitWithPredicate } from '../../core/shared/object-utils'

export function getVariablesInScope(
  elementPath: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
  variablesInScopeFromEditorState: VariablesInScope,
  jsxMetadata: ElementInstanceMetadataMap,
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

        const componentPropsInScope = getComponentPropsInScope(
          success.topLevelElements,
          elementPath,
          jsxMetadata,
        )
        varsInScope.push(componentPropsInScope)
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
    variables: generateVariableTypes(jsxComponentVariables),
  }
}

function getComponentPropsInScope(
  topLevelElements: TopLevelElement[],
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): AllVariablesInScope {
  const jsxComponent = topLevelElements.find((el) => isUtopiaJSXComponent(el)) as UtopiaJSXComponent
  const jsxComponentPropNamesUsed = jsxComponent?.propsUsed ?? []

  const jsxComponentProps = omitWithPredicate(
    getContainerPropsValue(elementPath, jsxMetadata),
    (key) => !jsxComponentPropNamesUsed.includes(key as string),
  )
  const name = jsxComponent?.name ?? 'Component'
  return {
    filePath: `${name}::props`,
    variables: generateVariableTypes(jsxComponentProps),
  }
}

function getContainerPropsValue(
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): VariableData {
  const elementPathString = toComponentId(getContainingComponent(elementPath))
  const containerMetadata = jsxMetadata[elementPathString]?.element.value
  const runtimeProps: VariableData = {}

  if (
    containerMetadata != null &&
    typeof containerMetadata !== 'string' &&
    isJSXElement(containerMetadata)
  ) {
    containerMetadata.props.forEach((prop) => {
      if (isJSXAttributesEntry(prop)) {
        const value = simplifyAttributeIfPossible(prop.value)
        if (!isJSXAttributeValue(value)) return
        runtimeProps[prop.key] = value.value
      }
    })
  }

  return runtimeProps
}

function generateVariableTypes(variables: VariableData): Variable[] {
  return Object.entries(variables).flatMap(([name, value]) => {
    const type = getTypeByValue(value)
    const variable = {
      name,
      value,
      type,
    }
    if (type === 'object' && value != null) {
      // iterate also first-level keys of object
      return [variable].concat(
        Object.entries(value).map(([key, innerValue]) => ({
          displayName: `${key}`,
          name: `${name}.${key}`,
          value: innerValue,
          type: getTypeByValue(innerValue),
          parent: variable,
          depth: 1,
        })),
      )
    } else {
      return variable
    }
  })
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
          variable.displayName ?? variable.name,
          [],
          null,
          { variableType: variable.type, depth: variable.depth },
        )
      }),
    }
  })
}

function getMatchingElementForVariable(variable: Variable): ComponentElementToInsert {
  return getMatchingElementForVariableInner(variable).component
}

function getMatchingElementForVariableInner(variable: Variable): InsertableComponentAndJSX {
  const originalVariableName = variable.parent != null ? variable.parent.name : variable.name
  switch (variable.type) {
    case 'string':
      return simpleInsertableComponentAndJsx('span', variable.name, originalVariableName)
    case 'number':
      return simpleInsertableComponentAndJsx('span', variable.name, originalVariableName)
    case 'boolean':
      return insertableComponentAndJSX(
        jsxConditionalExpressionWithoutUID(
          jsExpressionOtherJavaScriptSimple(variable.name, [originalVariableName]),
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
        originalVariableName,
      )
    case 'array':
      return arrayInsertableComponentAndJsx(variable)
    case 'image':
      return insertableComponentAndJSX(
        jsxElementWithoutUID(
          'img',
          jsxAttributesFromMap({
            src: jsExpressionOtherJavaScriptSimple(variable.name, [originalVariableName]),
          }),
          [],
        ),
        `<img src={${variable.name}} style={{width: 100, height: 100, contain: 'layout'}}/>`,
      )
    default:
      return simpleInsertableComponentAndJsx(
        'span',
        `JSON.stringify(${variable.name})`,
        originalVariableName,
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
  displayName?: string
  value?: unknown
  parent?: Variable
  depth?: number
}
