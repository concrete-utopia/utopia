import { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { ParsedPropertyControls, parsePropertyControls } from './property-controls-parser'
import { PropertyControls, getDefaultProps } from 'utopia-api'
import { isRight, foldEither, left } from '../shared/either'
import { forEachValue } from '../shared/object-utils'
import { descriptionParseError, ParseResult } from '../../utils/value-parser-utils'
import React from 'react'
import { joinSpecial } from '../shared/array-utils'
import { fastForEach } from '../shared/utils'
import {
  isResolvedNpmDependency,
  PossiblyUnversionedNpmDependency,
} from '../shared/npm-dependency-types'
import { getThirdPartyComponents } from '../third-party/third-party-components'
import {
  getJSXElementNameAsString,
  isIntrinsicHTMLElement,
  JSXElement,
  getJSXElementNameLastPart,
  isIntrinsicElement,
} from '../shared/element-template'
import {
  NodeModules,
  ParseSuccess,
  StaticElementPath,
  ElementPath,
} from '../shared/project-file-types'
import {
  getOpenUIJSFileKey,
  EditorState,
  withUnderlyingTarget,
  packageJsonFileFromProjectContents,
} from '../../components/editor/store/editor-state'
import { HtmlElementStyleObjectProps } from '../third-party/html-intrinsic-elements'
import { ProjectContentTreeRoot } from '../../components/assets'
import { importedFromWhere } from '../../components/editor/import-utils'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import { ReactThreeFiberControls } from './third-party-property-controls/react-three-fiber-controls'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import { getThirdPartyControlsIntrinsic } from './property-controls-local'

export interface FullNodeModulesUpdate {
  type: 'FULL_NODE_MODULES_UPDATE'
  nodeModules: NodeModules
}

export function fullNodeModulesUpdate(nodeModules: NodeModules): FullNodeModulesUpdate {
  return {
    type: 'FULL_NODE_MODULES_UPDATE',
    nodeModules: nodeModules,
  }
}

export interface PartialNodeModulesUpdate {
  type: 'PARTIAL_NODE_MODULES_UPDATE'
  nodeModules: NodeModules
}

export function partialNodeModulesUpdate(nodeModules: NodeModules): PartialNodeModulesUpdate {
  return {
    type: 'PARTIAL_NODE_MODULES_UPDATE',
    nodeModules: nodeModules,
  }
}

export type NodeModulesUpdate = FullNodeModulesUpdate | PartialNodeModulesUpdate

export function applyNodeModulesUpdate(
  currentNodeModules: NodeModules,
  update: NodeModulesUpdate,
): NodeModules {
  switch (update.type) {
    case 'FULL_NODE_MODULES_UPDATE':
      return update.nodeModules
    case 'PARTIAL_NODE_MODULES_UPDATE':
      return {
        ...currentNodeModules,
        ...update.nodeModules,
      }
    default:
      const _exhaustiveCheck: never = update
      throw new Error(`Unhandled type ${JSON.stringify(update)}`)
  }
}

export function combineUpdates(
  first: NodeModulesUpdate,
  second: NodeModulesUpdate,
): NodeModulesUpdate {
  switch (second.type) {
    case 'FULL_NODE_MODULES_UPDATE':
      return second
    case 'PARTIAL_NODE_MODULES_UPDATE':
      switch (first.type) {
        case 'FULL_NODE_MODULES_UPDATE':
          return fullNodeModulesUpdate({
            ...first.nodeModules,
            ...second.nodeModules,
          })
        case 'PARTIAL_NODE_MODULES_UPDATE':
          return partialNodeModulesUpdate({
            ...first.nodeModules,
            ...second.nodeModules,
          })
        default:
          const _exhaustiveCheck: never = first
          throw new Error(`Unhandled type ${JSON.stringify(first)}`)
      }
    default:
      const _exhaustiveCheck: never = second
      throw new Error(`Unhandled type ${JSON.stringify(second)}`)
  }
}

export interface GetPropertyControlsInfoMessage {
  nodeModulesUpdate: NodeModulesUpdate
  projectContents: ProjectContentTreeRoot
  updatedAndReverseDepFilenames: Array<string>
}

export function createGetPropertyControlsInfoMessage(
  nodeModulesUpdate: NodeModulesUpdate,
  projectContents: ProjectContentTreeRoot,
  updatedAndReverseDepFilenames: Array<string>,
): GetPropertyControlsInfoMessage {
  return {
    nodeModulesUpdate: nodeModulesUpdate,
    projectContents: projectContents,
    updatedAndReverseDepFilenames: updatedAndReverseDepFilenames,
  }
}

function parsedPropertyControlsForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  propertyControlsInfo: PropertyControlsInfo,
): ParseResult<ParsedPropertyControls> {
  const propertyControlsForFile = propertyControlsInfo[filePathNoExtension] ?? {}
  if (componentName in propertyControlsForFile) {
    return parsePropertyControls(propertyControlsForFile[componentName], 'includeSpecialProps')
  } else {
    return left(descriptionParseError(`No property controls for ${componentName}.`))
  }
}

interface DefaultPropertiesForComponentInFileResult {
  defaultProps: { [prop: string]: unknown }
  parsedControls: ParseResult<ParsedPropertyControls>
}

export function defaultPropertiesForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  propertyControlsInfo: PropertyControlsInfo,
): DefaultPropertiesForComponentInFileResult {
  const parsedPropertyControls = parsedPropertyControlsForComponentInFile(
    componentName,
    filePathNoExtension,
    propertyControlsInfo,
  )
  return {
    defaultProps: getDefaultPropsFromParsedControls(parsedPropertyControls),
    parsedControls: parsedPropertyControls,
  }
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

export function filterSpecialProp(propKey: string | number): boolean {
  return propKey !== 'style' && propKey !== 'css' && propKey !== 'className'
}
export function filterSpecialProps(props: Array<string>): Array<string> {
  return props.filter(filterSpecialProp)
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

export function removeIgnored(
  parsedPropertyControls: ParsedPropertyControls,
): ParsedPropertyControls {
  let result: ParsedPropertyControls = {}
  fastForEach(Object.keys(parsedPropertyControls), (key) => {
    const value = parsedPropertyControls[key]
    const shouldCopy = foldEither(
      (_) => true,
      (controlDescription) => {
        return controlDescription.control !== 'none'
      },
      value,
    )
    if (shouldCopy) {
      result[key] = value
    }
  })
  return result
}

export function getThirdPartyPropertyControls(
  packageName: string,
  packageVersion: string,
): PropertyControlsInfo {
  let propertyControlsInfo: PropertyControlsInfo = {}
  const componentDescriptor = getThirdPartyComponents(packageName, packageVersion)
  if (componentDescriptor != null) {
    fastForEach(componentDescriptor.components, (descriptor) => {
      if (descriptor.propertyControls != null) {
        const jsxElementName = getJSXElementNameAsString(descriptor.element.name)
        propertyControlsInfo[packageName] = {
          ...propertyControlsInfo[packageName],
          [jsxElementName]: {
            propertyControls: descriptor.propertyControls,
            componentInfo: {}, // TODO requiredImports
          },
        }
      }
    })
  }

  return propertyControlsInfo
}

export function getControlsForExternalDependencies(
  npmDependencies: ReadonlyArray<PossiblyUnversionedNpmDependency>,
): PropertyControlsInfo {
  let propertyControlsInfo: PropertyControlsInfo = {}
  fastForEach(npmDependencies, (dependency) => {
    if (isResolvedNpmDependency(dependency)) {
      propertyControlsInfo = {
        ...propertyControlsInfo,
        ...getThirdPartyPropertyControls(dependency.name, dependency.version),
      }
    }
  })
  return propertyControlsInfo
}

export function getPropertyControlsForTargetFromEditor(
  target: ElementPath,
  editor: EditorState,
): PropertyControls | null {
  const openFilePath = getOpenUIJSFileKey(editor)
  return getPropertyControlsForTarget(
    target,
    editor.propertyControlsInfo,
    openFilePath,
    editor.projectContents,
    editor.nodeModules.files,
  )
}

export function getPropertyControlsForTarget(
  target: ElementPath,
  propertyControlsInfo: PropertyControlsInfo,
  openFilePath: string | null,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): PropertyControls | null {
  return withUnderlyingTarget(
    target,
    projectContents,
    nodeModules,
    openFilePath,
    null,
    (
      success: ParseSuccess,
      element: JSXElement,
      underlyingTarget: StaticElementPath,
      underlyingFilePath: string,
    ) => {
      const importedFrom = importedFromWhere(
        underlyingFilePath,
        element.name.baseVariable,
        success.topLevelElements,
        success.imports,
      )

      let filenameForLookup: string | null = null
      if (importedFrom == null) {
        if (isIntrinsicElement(element.name)) {
          if (isIntrinsicHTMLElement(element.name)) {
            /**
             * We detected an intrinsic HTML Element (such as div, a, span, etc...)
             * for the sake of simplicity, we assume here that they all support the style prop. if we need more detailed
             * information for them, feel free to turn this into a real data structure that contains specific props for specific elements,
             * but for now, I just return a one-size-fits-all PropertyControls result here
             */
            return HtmlElementStyleObjectProps
          } else {
            // you can add more intrinsic (ie not imported) element types here
            return getThirdPartyControlsIntrinsic(
              element.name.baseVariable,
              propertyControlsInfo,
              projectContents,
            )
          }
        } else if (openFilePath != null) {
          filenameForLookup = openFilePath.replace(/\.(js|jsx|ts|tsx)$/, '')
        }
      } else {
        filenameForLookup = importedFrom.filePath
      }

      if (filenameForLookup == null) {
        return null
      } else {
        const absolutePath = absolutePathFromRelativePath(
          underlyingFilePath,
          false,
          filenameForLookup,
        )
        // If it's pointing at a path (as opposed to a package), strip off the filename extension.
        const trimmedPath = absolutePath.includes('/')
          ? absolutePath.replace(/\.(js|jsx|ts|tsx)$/, '')
          : absolutePath

        const nameLastPart = getJSXElementNameAsString(element.name)
        if (
          propertyControlsInfo[trimmedPath] != null &&
          propertyControlsInfo[trimmedPath][nameLastPart] != null
        ) {
          return propertyControlsInfo[trimmedPath][nameLastPart]
            .propertyControls as PropertyControls
        } else {
          return null
        }
      }
    },
  )
}

export const PropertyControlsInfoIFrameID = 'property-controls-info-frame'

let propertyControlsIFrameReady: boolean = false
let propertyControlsIFrameAvailable: boolean = false
let lastPropertyControlsInfoSendID: number | undefined = undefined
let queuedNodeModulesUpdate: NodeModulesUpdate | null = null

export function setPropertyControlsIFrameReady(value: boolean): void {
  propertyControlsIFrameReady = value
}

export function setPropertyControlsIFrameAvailable(value: boolean): void {
  propertyControlsIFrameAvailable = value
}

export function sendPropertyControlsInfoRequest(
  nodeModules: NodeModules,
  projectContents: ProjectContentTreeRoot,
  onlyProjectFiles: boolean,
  updatedAndReverseDepFilenames: Array<string>,
): void {
  let nodeModulesUpdate: NodeModulesUpdate
  if (onlyProjectFiles) {
    let notNodeModulesFiles: NodeModules = {}
    forEachValue((value, key) => {
      if (typeof key === 'string' && !key.startsWith('/node_modules')) {
        notNodeModulesFiles[key] = value
      }
    }, nodeModules)
    nodeModulesUpdate = partialNodeModulesUpdate(notNodeModulesFiles)
  } else {
    nodeModulesUpdate = fullNodeModulesUpdate(nodeModules)
  }

  // Include any potentially not yet sent updates ahead of any potential scheduling so that
  // later updates are applied in the correct order.
  queuedNodeModulesUpdate =
    queuedNodeModulesUpdate == null
      ? nodeModulesUpdate
      : combineUpdates(queuedNodeModulesUpdate, nodeModulesUpdate)

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
            if (queuedNodeModulesUpdate != null) {
              contentWindow.postMessage(
                createGetPropertyControlsInfoMessage(
                  queuedNodeModulesUpdate,
                  projectContents,
                  updatedAndReverseDepFilenames,
                ),
                '*',
              )
              queuedNodeModulesUpdate = null
            }
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
