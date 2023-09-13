import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { ParsedPropertyControls } from './property-controls-parser'
import type { PropertyControls } from 'utopia-api/core'
import { ImportType } from 'utopia-api/core'
import { isRight, foldEither, left, maybeEitherToMaybe, eitherToMaybe } from '../shared/either'
import { forEachValue } from '../shared/object-utils'
import { descriptionParseError, ParseResult } from '../../utils/value-parser-utils'
import type { JSXElementChild } from '../shared/element-template'
import {
  getJSXElementNameAsString,
  isIntrinsicHTMLElement,
  JSXElement,
  isIntrinsicElement,
  isJSXElement,
} from '../shared/element-template'
import type {
  NodeModules,
  ParseSuccess,
  StaticElementPath,
  ElementPath,
} from '../shared/project-file-types'
import type { DerivedState, EditorState } from '../../components/editor/store/editor-state'
import {
  getOpenUIJSFileKey,
  withUnderlyingTarget,
} from '../../components/editor/store/editor-state'
import { HtmlElementStyleObjectProps } from '../third-party/html-intrinsic-elements'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { importedFromWhere } from '../../components/editor/import-utils'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import { getThirdPartyControlsIntrinsic } from './property-controls-local'
import type { RemixRoutingTable } from '../../components/editor/store/remix-derived-data'

export function propertyControlsForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  propertyControlsInfo: PropertyControlsInfo,
): PropertyControls {
  const propertyControlsForFile = propertyControlsInfo[filePathNoExtension] ?? {}
  const propertyControlsForComponent = propertyControlsForFile[componentName]?.properties
  return propertyControlsForComponent ?? {}
}

export function getPropertyControlsForTargetFromEditor(
  target: ElementPath,
  editor: EditorState,
  derivedState: DerivedState,
): PropertyControls | null {
  const openFilePath = getOpenUIJSFileKey(editor)
  return getPropertyControlsForTarget(
    target,
    editor.propertyControlsInfo,
    openFilePath,
    editor.projectContents,
    editor.nodeModules.files,
    derivedState.remixData?.routingTable ?? null,
  )
}

export function getPropertyControlsForTarget(
  target: ElementPath,
  propertyControlsInfo: PropertyControlsInfo,
  openFilePath: string | null,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  remixRoutingTable: RemixRoutingTable | null,
): PropertyControls | null {
  return withUnderlyingTarget(
    target,
    projectContents,
    nodeModules,
    remixRoutingTable,
    openFilePath,
    null,
    (
      success: ParseSuccess,
      element: JSXElementChild,
      underlyingTarget: StaticElementPath,
      underlyingFilePath: string,
    ) => {
      if (isJSXElement(element)) {
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

          const originalName =
            importedFrom?.type === 'IMPORTED_ORIGIN' ? importedFrom.exportedName : null
          const nameAsString = originalName ?? getJSXElementNameAsString(element.name)
          return propertyControlsInfo[trimmedPath]?.[nameAsString]?.properties
        }
      } else {
        return null
      }
    },
  )
}

export function hasStyleControls(propertyControls: PropertyControls): boolean {
  return propertyControls['style']?.control === 'style-controls'
}

export const specialPropertiesToIgnore: Array<string> = ['style', 'children']
