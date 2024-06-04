import type { ElementPath } from '../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../editor/store/editor-state'
import type { ProjectContentTreeRoot } from '../assets'
import type {
  ElementInstanceMetadataMap,
  ArbitraryJSBlock,
  TopLevelElement,
  UtopiaJSXComponent,
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
import type { FileRootPath, VariableData } from '../canvas/ui-jsx-canvas'
import { type VariablesInScope } from '../canvas/ui-jsx-canvas'
import { getContainingComponent, toComponentId } from '../../core/shared/element-path'
import {
  type InsertableComponentGroup,
  type InsertableVariableType,
  insertableComponentGroupProjectComponent,
  insertableVariable,
} from './project-components'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { isImage } from '../../core/shared/utils'
import { type ComponentElementToInsert } from '../custom-code/code-file'
import { omitWithPredicate } from '../../core/shared/object-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isLeft } from '../../core/shared/either'
import { findContainingComponent } from '../../core/model/element-template-utils'

export function getVariablesInScope(
  elementPath: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
  variablesInScopeFromEditorState: VariablesInScope,
  jsxMetadata: ElementInstanceMetadataMap,
): AllVariablesInScope[] {
  return withUnderlyingTarget(elementPath, projectContents, [], (success) => {
    let varsInScope = []

    if (elementPath !== null) {
      const containingComponent = findContainingComponent(success.topLevelElements, elementPath)
      const componentScopedVariables = getVariablesFromComponent(
        containingComponent,
        elementPath,
        variablesInScopeFromEditorState,
      )
      varsInScope.push(componentScopedVariables)

      const componentPropsInScope = getComponentPropsInScope(
        containingComponent,
        elementPath,
        jsxMetadata,
      )
      varsInScope.push(componentPropsInScope)
    }

    /** for future reference - adding variables from top level **/
    // const topLevelVariables = getTopLevelVariables(success.topLevelElements, underlyingFilePath)
    // varsInScope.push(topLevelVariables)

    return varsInScope
  })
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
  jsxComponent: UtopiaJSXComponent | null,
  elementPath: ElementPath,
  variablesInScopeFromEditorState: VariablesInScope,
): AllVariablesInScope {
  const elementPathString = toComponentId(elementPath)
  const jsxComponentVariables = variablesInScopeFromEditorState[elementPathString] ?? {}
  return {
    filePath: jsxComponent?.name ?? 'Component',
    variables: generateVariableTypes(jsxComponentVariables),
  }
}

function getComponentPropsInScope(
  jsxComponent: UtopiaJSXComponent | null,
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): AllVariablesInScope {
  const jsxComponentPropNamesDeclared = jsxComponent?.propsUsed ?? []

  const jsxComponentPropsPassed = omitWithPredicate(
    getContainerPropsValue(elementPath, jsxMetadata),
    (key) => typeof key !== 'string' || !jsxComponentPropNamesDeclared.includes(key),
  )

  const name = jsxComponent?.name ?? 'Component'
  return {
    filePath: `${name}::props`,
    variables: generateVariableTypes(jsxComponentPropsPassed),
  }
}

function getContainerPropsValue(
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): VariableData {
  const containerMetadata = MetadataUtils.findElementByElementPath(
    jsxMetadata,
    getContainingComponent(elementPath),
  )
  const runtimeProps: VariableData = {}

  if (containerMetadata == null || isLeft(containerMetadata.element)) {
    return runtimeProps
  }

  const containerElement = containerMetadata.element.value

  if (isJSXElement(containerElement)) {
    containerElement.props.forEach((prop) => {
      if (isJSXAttributesEntry(prop)) {
        const value = simplifyAttributeIfPossible(prop.value)
        if (isJSXAttributeValue(value)) {
          runtimeProps[prop.key] = {
            spiedValue: value.value as unknown,
            insertionCeiling: elementPath,
          }
        }
      }
    })
  }

  return runtimeProps
}

function generateVariableTypes(variables: VariableData): Variable[] {
  return Object.entries(variables).flatMap(([name, valueMetadata]) => {
    const value = valueMetadata.spiedValue
    const insertionCeiling = valueMetadata.insertionCeiling
    const type = getTypeByValue(value)
    const variable: Variable = {
      name,
      value,
      type,
      depth: 0,
      insertionCeiling: insertionCeiling,
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
          insertionCeiling: insertionCeiling,
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
        return insertableVariable(
          emptyImports(),
          () => getMatchingElementForVariable(variable),
          variable.displayName ?? variable.name,
          [],
          null,
          variable.type,
          variable.depth,
          variable.name,
          variable.insertionCeiling,
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

function getTypeByValue(value: unknown): InsertableVariableType {
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
    depth: variable.depth + 1,
    insertionCeiling: variable.insertionCeiling,
  }).jsx
  const arrayElementString = `${variable.name}.map((item) => (${innerElementString}))`
  const arrayElement = jsxFragmentWithoutUID([jsxTextBlock(`{${arrayElementString}}`)], true)
  return insertableComponentAndJSX(arrayElement, arrayElementString)
}

function getArrayType(arrayVariable: Variable): InsertableVariableType {
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

interface Variable {
  name: string
  type: InsertableVariableType
  displayName?: string
  value?: unknown
  parent?: Variable
  depth: number
  insertionCeiling: FileRootPath | ElementPath
}
