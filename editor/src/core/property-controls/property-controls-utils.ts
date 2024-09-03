import type {
  ComponentDescriptor,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import type { JSXElementChild } from '../shared/element-template'
import {
  getJSXElementNameAsString,
  isIntrinsicHTMLElement,
  isIntrinsicElement,
  isJSXElement,
  jsxElementName,
} from '../shared/element-template'
import type { ParseSuccess, StaticElementPath, ElementPath } from '../shared/project-file-types'
import type { EditorState } from '../../components/editor/store/editor-state'
import { withUnderlyingTarget } from '../../components/editor/store/editor-state'
import { HtmlElementStyleObjectProps } from '../third-party/html-intrinsic-elements'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { importedFromWhere } from '../../components/editor/import-utils'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import { getThirdPartyControlsIntrinsic } from './property-controls-local'
import type { PropertyControls } from '../../components/custom-code/internal-property-controls'
import { dropFileExtension } from '../shared/file-utils'
import type { Styling } from 'utopia-api'
import { StylingOptions } from 'utopia-api'
import { intersection } from '../shared/set-utils'
import { getFilePathMappings } from '../model/project-file-utils'
import { valueDependentCache } from '../shared/memoize'
import * as EP from '../shared/element-path'
import { shallowEqual } from '../shared/equality-utils'

export function propertyControlsForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  propertyControlsInfo: PropertyControlsInfo,
): PropertyControls | null {
  const propertyControlsForFile = propertyControlsInfo[filePathNoExtension] ?? {}
  const propertyControlsForComponent = propertyControlsForFile[componentName]
  if (propertyControlsForComponent == null) {
    return null
  }
  return propertyControlsForComponent.properties
}

export function getPropertyControlsForTargetFromEditor(
  target: ElementPath,
  editor: EditorState,
): PropertyControls | null {
  return getPropertyControlsForTarget(target, editor.propertyControlsInfo, editor.projectContents)
}

export function getPropertyControlsForTarget(
  target: ElementPath,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): PropertyControls | null {
  const filePathMappings = getFilePathMappings(projectContents)
  return withUnderlyingTarget(
    target,
    projectContents,
    null,
    (
      success: ParseSuccess,
      element: JSXElementChild,
      _: StaticElementPath,
      underlyingFilePath: string,
    ) => {
      if (isJSXElement(element)) {
        const importedFrom = importedFromWhere(
          filePathMappings,
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
          } else {
            filenameForLookup = underlyingFilePath.replace(/\.(js|jsx|ts|tsx)$/, '')
          }
        } else {
          filenameForLookup = importedFrom.filePath
        }

        if (filenameForLookup == null) {
          return null
        } else {
          const originalName =
            importedFrom?.type === 'IMPORTED_ORIGIN' ? importedFrom.exportedName : null
          const jsxName =
            originalName != null
              ? jsxElementName(originalName, element.name.propertyPath.propertyElements)
              : element.name
          const nameAsString = getJSXElementNameAsString(jsxName)

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

export const getComponentDescriptorForTarget = valueDependentCache(
  getComponentDescriptorForTargetInner,
  EP.toString,
  {
    equality: (s1, s2) =>
      s1.projectContents === s2.projectContents &&
      s1.propertyControlsInfo === s2.propertyControlsInfo,
  },
)

const FileExtRegExp = /\.(js|jsx|ts|tsx)$/

function getComponentDescriptorForTargetInner(
  {
    projectContents,
    propertyControlsInfo,
  }: {
    propertyControlsInfo: PropertyControlsInfo
    projectContents: ProjectContentTreeRoot
  },
  target: ElementPath,
): ComponentDescriptor | null {
  const filePathMappings = getFilePathMappings(projectContents)
  return withUnderlyingTarget(
    target,
    projectContents,
    null,
    (
      success: ParseSuccess,
      element: JSXElementChild,
      _: StaticElementPath,
      underlyingFilePath: string,
    ) => {
      if (isJSXElement(element)) {
        if (isIntrinsicElement(element.name)) {
          return null
        }

        const importedFrom = importedFromWhere(
          filePathMappings,
          underlyingFilePath,
          element.name.baseVariable,
          success.topLevelElements,
          success.imports,
        )

        let filenameForLookup: string | null = null
        if (importedFrom == null) {
          filenameForLookup = underlyingFilePath.replace(FileExtRegExp, '')
        } else {
          filenameForLookup = importedFrom.filePath
        }

        if (filenameForLookup == null) {
          return null
        } else {
          const originalName =
            importedFrom?.type === 'IMPORTED_ORIGIN' ? importedFrom.exportedName : null
          const jsxName =
            originalName != null
              ? jsxElementName(originalName, element.name.propertyPath.propertyElements)
              : element.name
          const nameAsString = getJSXElementNameAsString(jsxName)

          const componentDescriptor = propertyControlsInfo[filenameForLookup]?.[nameAsString]

          // if the filename works as it is, then it is either a package name or an absolute file name and
          // we can just use it as it is
          if (componentDescriptor != null) {
            return componentDescriptor
          }

          // We need to create the absolute path to the file to look up the property controls
          const absolutePath = absolutePathFromRelativePath(
            underlyingFilePath,
            false,
            filenameForLookup,
          )

          const trimmedPath = absolutePath.includes('/')
            ? absolutePath.replace(FileExtRegExp, '')
            : absolutePath

          return propertyControlsInfo[trimmedPath]?.[nameAsString]
        }
      } else {
        return null
      }
    },
  )
}

export function getRegisteredComponent(
  component: string,
  moduleName: string,
  propertyControlsInfo: PropertyControlsInfo,
): ComponentDescriptor | null {
  const registeredModule =
    propertyControlsInfo[moduleName] ?? propertyControlsInfo[dropFileExtension(moduleName)]
  return registeredModule?.[component]
}

export function hasStyleControls(propertyControls: PropertyControls): boolean {
  return propertyControls['style']?.control === 'style-controls'
}

export const specialPropertiesToIgnore: Array<string> = ['style']

export type InspectorSectionPreference = 'layout' | 'layout-system' | 'visual' | 'typography'

export type TypedInspectorSpec = { type: 'all' } | { type: 'sections'; sections: Styling[] }

export function getInspectorPreferencesForTargets(
  targets: ElementPath[],
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): InspectorSectionPreference[] {
  const inspectorPreferences = targets.map((target) => {
    const controls = getComponentDescriptorForTarget(
      { propertyControlsInfo, projectContents },
      target,
    )
    if (controls == null || controls.inspector == null) {
      return { type: 'all' }
    }

    if (controls.inspector.type === 'shown') {
      return { type: 'sections', sections: controls.inspector.sections }
    }

    return { type: 'sections', sections: [] }
  })

  let sectionsToShow: Set<Styling> = new Set(StylingOptions)
  inspectorPreferences.forEach((preference) => {
    if (preference.type === 'all') {
      return
    }

    // only returning the sections that are supported by all the elements, so
    // that the editing UI we expose only makes valid edits to all the
    // components
    sectionsToShow = intersection([sectionsToShow, new Set(preference.sections)])
  })

  return [...sectionsToShow]
}

export const AdvancedFolderLabel = 'Advanced'

const advancedFolderLabel = AdvancedFolderLabel.toLowerCase()
export const isAdvancedFolderLabel = (title: string | undefined) =>
  title != null && title.toLowerCase() === advancedFolderLabel
