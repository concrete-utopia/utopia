import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import type { JSXElementChild } from '../shared/element-template'
import {
  getJSXElementNameAsString,
  isIntrinsicHTMLElement,
  isIntrinsicElement,
  isJSXElement,
} from '../shared/element-template'
import type { ParseSuccess, StaticElementPath, ElementPath } from '../shared/project-file-types'
import type { EditorState } from '../../components/editor/store/editor-state'
import {
  getOpenUIJSFileKey,
  withUnderlyingTarget,
} from '../../components/editor/store/editor-state'
import { HtmlElementStyleObjectProps } from '../third-party/html-intrinsic-elements'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { importedFromWhere } from '../../components/editor/import-utils'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import { getThirdPartyControlsIntrinsic } from './property-controls-local'
import type { PropertyControls } from '../../components/custom-code/internal-property-controls'

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
): PropertyControls | null {
  const openFilePath = getOpenUIJSFileKey(editor)
  return getPropertyControlsForTarget(
    target,
    editor.propertyControlsInfo,
    openFilePath,
    editor.projectContents,
  )
}

export function getPropertyControlsForTarget(
  target: ElementPath,
  propertyControlsInfo: PropertyControlsInfo,
  openFilePath: string | null,
  projectContents: ProjectContentTreeRoot,
): PropertyControls | null {
  return withUnderlyingTarget(
    target,
    projectContents,
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
          const originalName =
            importedFrom?.type === 'IMPORTED_ORIGIN' ? importedFrom.exportedName : null
          const nameAsString = originalName ?? getJSXElementNameAsString(element.name)

          const props = propertyControlsInfo[filenameForLookup]?.[nameAsString]?.properties

          // if the filename works as it is, then it is either a package name or an absolute file name and
          // we can just use it as it is
          if (props != null) {
            return props
          }

          // We need to create the absolute path to the file to look up the property controls
          const absolutePath = absolutePathFromRelativePath(
            underlyingFilePath,
            false,
            filenameForLookup,
          )

          const trimmedPath = absolutePath.includes('/')
            ? absolutePath.replace(/\.(js|jsx|ts|tsx)$/, '')
            : absolutePath

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
