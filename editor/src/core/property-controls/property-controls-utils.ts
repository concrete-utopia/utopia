import { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { ParsedPropertyControls } from './property-controls-parser'
import { PropertyControls, getDefaultProps, ImportType } from 'utopia-api'
import { isRight, foldEither, left, maybeEitherToMaybe, eitherToMaybe } from '../shared/either'
import { forEachValue } from '../shared/object-utils'
import { descriptionParseError, ParseResult } from '../../utils/value-parser-utils'
import {
  getJSXElementNameAsString,
  isIntrinsicHTMLElement,
  JSXElement,
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
} from '../../components/editor/store/editor-state'
import { HtmlElementStyleObjectProps } from '../third-party/html-intrinsic-elements'
import { ProjectContentTreeRoot } from '../../components/assets'
import { importedFromWhere } from '../../components/editor/import-utils'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import { getThirdPartyControlsIntrinsic } from './property-controls-local'

function parsedPropertyControlsForComponentInFile(
  componentName: string,
  filePathNoExtension: string,
  propertyControlsInfo: PropertyControlsInfo,
): ParseResult<ParsedPropertyControls> {
  const propertyControlsForFile = propertyControlsInfo[filePathNoExtension] ?? {}
  const propertyControlsForComponent = propertyControlsForFile[componentName]?.propertyControls
  return (
    propertyControlsForComponent ??
    left(descriptionParseError(`No property controls for ${componentName}.`))
  )
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

export function getPropertyControlsForTargetFromEditor(
  target: ElementPath,
  editor: EditorState,
): ParseResult<ParsedPropertyControls> | null {
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
): ParseResult<ParsedPropertyControls> | null {
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
        return propertyControlsInfo[trimmedPath]?.[nameLastPart]?.propertyControls
      }
    },
  )
}

export function hasStyleControls(propertyControls: ParseResult<ParsedPropertyControls>): boolean {
  return foldEither(
    (_parseFailed) => false,
    (parsedPropertyControls) => {
      const styleControls = parsedPropertyControls['style']
      if (styleControls == null) {
        return false
      } else {
        return foldEither(
          (_) => false,
          (r) => r.control === 'style-controls',
          styleControls,
        )
      }
    },
    propertyControls,
  )
}
