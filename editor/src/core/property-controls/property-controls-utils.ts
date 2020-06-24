import {
  CodeResultCache,
  PropertyControlsInfo,
  UtopiaRequireFn,
} from '../../components/custom-code/code-file'
import {
  parsePropertyControlsForFile,
  ParsedPropertyControls,
  parsePropertyControls,
} from './property-controls-parser'
import { PropertyControls, getDefaultProps, ControlDescription } from 'utopia-api'
import { isRight, foldEither } from '../shared/either'
import { forEachValue } from '../shared/object-utils'
import { ParseResult } from '../../utils/value-parser-utils'
import * as React from 'react'
import { joinSpecial } from '../shared/array-utils'
import { fastForEach } from '../shared/utils'
import { AntdControls } from './third-party-property-controls/antd-controls'
import { NpmDependency } from '../shared/npm-dependency-types'
import { getThirdPartyComponents } from '../third-party/third-party-components'
import { getJSXElementNameAsString } from '../shared/element-template'

export function defaultPropertiesForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  codeResultCache: CodeResultCache | null,
): { [prop: string]: unknown } {
  const propertyControlsForFile = codeResultCache?.propertyControlsInfo[filePathNoExtension] ?? {}
  const parsedPropertyControlsForFile = parsePropertyControlsForFile(propertyControlsForFile)
  const parsedPropertyControls = parsedPropertyControlsForFile[componentName]
  return parsedPropertyControls == null
    ? {}
    : getDefaultPropsFromParsedControls(parsedPropertyControls)
}

export function defaultPropertiesForComponent(
  component: React.ComponentType<any> & { propertyControls?: PropertyControls },
): { [prop: string]: unknown } {
  const propertyControls = component.propertyControls == null ? {} : component.propertyControls
  const parsedPropertyControls = parsePropertyControls(propertyControls)
  return getDefaultPropsFromParsedControls(parsedPropertyControls)
}

export function getDefaultPropsFromParsedControls(
  parsedControls: ParseResult<ParsedPropertyControls>,
): { [prop: string]: unknown } {
  let safePropertyControls: PropertyControls = {}
  if (isRight(parsedControls)) {
    forEachValue((parsedControl, propKey) => {
      if (isRight(parsedControl)) {
        safePropertyControls[propKey] = parsedControl.value
      }
    }, parsedControls.value)
  }
  return getDefaultProps(safePropertyControls)
}

export function getMissingPropertyControlsWarning(
  propsWithoutControls: Array<string>,
): string | undefined {
  if (propsWithoutControls.length < 1) {
    return undefined
  } else {
    return `There are no property controls for these props: ${joinSpecial(
      propsWithoutControls,
      ', ',
      ' & ',
    )}`
  }
}

export function findMissingDefaultsAndGetWarning(
  knownProps: Array<string>,
  propsWithDefaults: { [prop: string]: unknown },
): string | undefined {
  const propsMissingDefaults = findMissingDefaults(knownProps, propsWithDefaults)
  return getMissingDefaultsWarning(propsMissingDefaults)
}

export function findMissingDefaults(
  knownProps: Array<string>,
  propsWithDefaults: { [prop: string]: unknown },
): Array<string> {
  const defaultPropKeys = Object.keys(propsWithDefaults)
  const filteredKnownProps = filterSpecialProps(knownProps)
  return filteredKnownProps.filter((propKey) => !defaultPropKeys.includes(propKey))
}

export function filterSpecialProps(props: Array<string>): Array<string> {
  return props.filter(
    (propKey) => propKey !== 'style' && propKey !== 'css' && propKey !== 'className',
  )
}

export function getMissingDefaultsWarning(propsWithoutDefaults: Array<string>): string | undefined {
  if (propsWithoutDefaults.length < 1) {
    return undefined
  } else if (propsWithoutDefaults.length === 1) {
    return `The prop ${propsWithoutDefaults[0]} doesn't have a default value.`
  } else {
    return `These props don't have default values: ${joinSpecial(
      propsWithoutDefaults,
      ', ',
      ' & ',
    )}`
  }
}

export function getDescriptionUnsetOptionalFields(
  controlDescription: ControlDescription,
): Array<string> {
  let result: Array<string> = []
  function addIfFieldEmpty<T extends ControlDescription, K extends keyof T & string>(
    description: T,
    fieldName: K,
  ): void {
    if (description[fieldName] == null) {
      result.push(fieldName)
    }
  }
  addIfFieldEmpty(controlDescription, 'title')
  switch (controlDescription.type) {
    case 'array':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      addIfFieldEmpty(controlDescription, 'maxCount')
      break
    case 'boolean':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      addIfFieldEmpty(controlDescription, 'disabledTitle')
      addIfFieldEmpty(controlDescription, 'enabledTitle')
      break
    case 'color':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      break
    case 'componentinstance':
      break
    case 'enum':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      addIfFieldEmpty(controlDescription, 'optionTitles')
      addIfFieldEmpty(controlDescription, 'displaySegmentedControl')
      break
    case 'eventhandler':
    case 'ignore':
    case 'image':
      break
    case 'number':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      addIfFieldEmpty(controlDescription, 'max')
      addIfFieldEmpty(controlDescription, 'min')
      addIfFieldEmpty(controlDescription, 'unit')
      addIfFieldEmpty(controlDescription, 'step')
      addIfFieldEmpty(controlDescription, 'displayStepper')
      break
    case 'object':
      break
    case 'options':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      break
    case 'string':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      addIfFieldEmpty(controlDescription, 'placeholder')
      addIfFieldEmpty(controlDescription, 'obscured')
      break
    case 'popuplist':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      break
    case 'slider':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      break
    default:
      // Usual exhaustiveness check is invalid here for some reason.
      throw new Error(`Unhandled type ${JSON.stringify(controlDescription)}`)
  }
  return result
}

export function removeIgnored(
  parsedPropertyControls: ParsedPropertyControls,
): ParsedPropertyControls {
  let result: ParsedPropertyControls = {}
  fastForEach(Object.keys(parsedPropertyControls), (key) => {
    const value = parsedPropertyControls[key]
    const shouldCopy = foldEither(
      (_) => true,
      (controlDescription) => {
        return controlDescription.type !== 'ignore'
      },
      value,
    )
    if (shouldCopy) {
      result[key] = value
    }
  })
  return result
}

export function getControlsForExternalDependencies(
  npmDependencies: NpmDependency[],
): PropertyControlsInfo {
  let propertyControlsInfo: PropertyControlsInfo = {}
  fastForEach(npmDependencies, (dependency) => {
    const componentDescriptor = getThirdPartyComponents(dependency.name, dependency.version)
    if (componentDescriptor != null) {
      fastForEach(componentDescriptor.components, (descriptor) => {
        if (descriptor.propertyControls != null) {
          const jsxElementName = getJSXElementNameAsString(descriptor.element.name)
          propertyControlsInfo[dependency.name] = {
            ...propertyControlsInfo[dependency.name],
            [jsxElementName]: descriptor.propertyControls,
          }
        }
      })
    }
  })
  return propertyControlsInfo
}
