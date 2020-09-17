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
import { forEachValue, objectMap } from '../shared/object-utils'
import { ParseResult } from '../../utils/value-parser-utils'
import * as React from 'react'
import { joinSpecial, pluck } from '../shared/array-utils'
import { fastForEach } from '../shared/utils'
import { NpmDependency } from '../shared/npm-dependency-types'
import { getThirdPartyComponents } from '../third-party/third-party-components'
import { getJSXElementNameAsString, isIntrinsicHTMLElement } from '../shared/element-template'
import {
  esCodeFile,
  isEsCodeFile,
  NodeModules,
  ProjectContents,
  TemplatePath,
} from '../shared/project-file-types'
import {
  getOpenImportsFromState,
  getOpenUtopiaJSXComponentsFromState,
  getOpenUIJSFileKey,
  EditorState,
} from '../../components/editor/store/editor-state'
import { MetadataUtils } from '../model/element-metadata-utils'
import { HtmlElementStyleObjectProps } from '../third-party/html-intrinsic-elements'
import { ExportsInfo } from '../workers/ts/ts-worker'

export interface GetPropertyControlsInfoMessage {
  exportsInfo: ReadonlyArray<ExportsInfo>
  nodeModules: NodeModules
  projectContents: ProjectContents
}

export function createGetPropertyControlsInfoMessage(
  exportsInfo: ReadonlyArray<ExportsInfo>,
  nodeModules: NodeModules,
  projectContents: ProjectContents,
): GetPropertyControlsInfoMessage {
  return {
    exportsInfo: exportsInfo,
    nodeModules: nodeModules,
    projectContents: projectContents,
  }
}

export function defaultPropertiesForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  propertyControlsInfo: PropertyControlsInfo,
): { [prop: string]: unknown } {
  const propertyControlsForFile = propertyControlsInfo[filePathNoExtension] ?? {}
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
    case 'styleobject':
      break
    case 'popuplist':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      break
    case 'slider':
      addIfFieldEmpty(controlDescription, 'defaultValue')
      break
    case 'union':
      break
    default:
      const _exhaustiveCheck: never = controlDescription
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
  npmDependencies: ReadonlyArray<NpmDependency>,
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

export function getPropertyControlsForTarget(
  target: TemplatePath,
  editor: EditorState,
): PropertyControls | null {
  const propertyControlsInfo = editor.propertyControlsInfo
  const imports = getOpenImportsFromState(editor)
  const openFilePath = getOpenUIJSFileKey(editor)
  const rootComponents = getOpenUtopiaJSXComponentsFromState(editor)
  const tagName = MetadataUtils.getJSXElementTagName(
    target,
    rootComponents,
    editor.jsxMetadataKILLME,
  )
  const importedName = MetadataUtils.getJSXElementBaseName(
    target,
    rootComponents,
    editor.jsxMetadataKILLME,
  )
  const jsxName = MetadataUtils.getJSXElementName(target, rootComponents, editor.jsxMetadataKILLME)
  if (importedName != null && tagName != null) {
    // TODO default and star imports
    let filename = Object.keys(imports).find((key) => {
      return pluck(imports[key].importedFromWithin, 'name').includes(importedName)
    })
    if (filename == null && jsxName != null && isIntrinsicHTMLElement(jsxName)) {
      /**
       * We detected an intrinsic HTML Element (such as div, a, span, etc...)
       * for the sake of simplicity, we assume here that they all support the style prop. if we need more detailed
       * information for them, feel free to turn this into a real data structure that contains specific props for specific elements,
       * but for now, I just return a one-size-fits-all PropertyControls result here
       */
      return HtmlElementStyleObjectProps
    }
    if (filename == null && openFilePath != null) {
      filename = openFilePath.replace(/\.(js|jsx|ts|tsx)$/, '')
    }
    if (filename != null) {
      // TODO figure out absolute filepath
      const absoluteFilePath = filename.startsWith('.') ? `${filename.slice(1)}` : `${filename}`
      if (
        propertyControlsInfo[absoluteFilePath] != null &&
        propertyControlsInfo[absoluteFilePath][tagName] != null
      ) {
        return propertyControlsInfo[absoluteFilePath][tagName] as PropertyControls
      } else {
        return null
      }
    } else {
      return null
    }
  } else {
    return null
  }
}

export const PropertyControlsInfoIFrameID = 'property-controls-info-frame'

let propertyControlsIFrameReady: boolean = false
let propertyControlsIFrameAvailable: boolean = false
let lastPropertyControlsInfoSendID: number | undefined = undefined

export function setPropertyControlsIFrameReady(value: boolean): void {
  propertyControlsIFrameReady = value
}

export function setPropertyControlsIFrameAvailable(value: boolean): void {
  propertyControlsIFrameAvailable = value
}

export function sendPropertyControlsInfoRequest(
  exportsInfo: ReadonlyArray<ExportsInfo>,
  nodeModules: NodeModules,
  projectContents: ProjectContents,
): void {
  function scheduleSend(): void {
    if (propertyControlsIFrameAvailable) {
      window.clearTimeout(lastPropertyControlsInfoSendID)
      lastPropertyControlsInfoSendID = window.setTimeout(async () => sendToIFrame(), 500)
    }
  }

  function sendToIFrame(): void {
    // Prevent a scheduled send from firing.
    window.clearTimeout(lastPropertyControlsInfoSendID)

    if (propertyControlsIFrameReady) {
      const propertyControlsInfoElement = document.getElementById(PropertyControlsInfoIFrameID)
      if (propertyControlsInfoElement == null) {
        scheduleSend()
      } else {
        const iFramePropertyControlsInfoElement = (propertyControlsInfoElement as any) as HTMLIFrameElement
        const contentWindow = iFramePropertyControlsInfoElement.contentWindow
        if (contentWindow == null) {
          scheduleSend()
        } else {
          try {
            // Need to clear evalResultCache because it may contain things which are not serializable.
            const clearedNodeModules: NodeModules = objectMap((nodeModule) => {
              if (isEsCodeFile(nodeModule)) {
                return esCodeFile(nodeModule.fileContents, null)
              } else {
                return nodeModule
              }
            }, nodeModules)
            contentWindow.postMessage(
              createGetPropertyControlsInfoMessage(
                exportsInfo,
                clearedNodeModules,
                projectContents,
              ),
              '*',
            )
          } catch (exception) {
            // Don't nuke the editor if there's an exception posting the message.
            // This can happen if a value can't be cloned when posted.
            console.error('Error sending message for property controls info.', exception)
          }
        }
      }
    } else {
      scheduleSend()
    }
  }

  // Initialise the first call.
  sendToIFrame()
}
