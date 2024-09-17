import * as MimeTypes from 'mime-types'
import pathParse from 'path-parse'
import * as PP from '../shared/property-path'
import { isText } from 'istextorbinary'
import { intrinsicHTMLElementNamesAsStrings } from '../shared/dom-utils'
import Utils from '../../utils/utils'
import type {
  Directory,
  HighlightBoundsForUids,
  ImageFile,
  Imports,
  ParsedTextFile,
  ParseSuccess,
  ProjectFile,
  ProjectFileType,
  TextFile,
  AssetFile,
  TextFileContents,
  HighlightBoundsWithFileForUids,
  RevisionsStateType,
} from '../shared/project-file-types'
import {
  RevisionsState,
  foldParsedTextFile,
  isTextFile,
  textFile,
  textFileContents,
  forEachParseSuccess,
  isParseSuccess,
  isExportDefaultFunctionOrClass,
  isExportFunction,
  isExportDefault,
  isImageFile,
  isAssetFile,
} from '../shared/project-file-types'
import type {
  JSXElementChild,
  JSXElementName,
  TopLevelElement,
  UtopiaJSXComponent,
  ImportInfo,
  ElementInstanceMetadata,
} from '../shared/element-template'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  getJSXElementNameLastPart,
  createImportedFrom,
  createNotImported,
  isIntrinsicElement,
  isIntrinsicElementFromString,
  isIntrinsicHTMLElement,
  isIntrinsicHTMLElementString,
  isImportedOrigin,
  isSameFileOrigin,
} from '../shared/element-template'
import {
  sceneMetadata as _sceneMetadata,
  fishOutUtopiaCanvasFromTopLevelElements,
  EmptyUtopiaCanvasComponent,
} from './scene-utils'
import { mapDropNulls, pluck } from '../shared/array-utils'
import { forEachValue, propOrNull } from '../shared/object-utils'
import type { ProjectContentsTree, ProjectContentTreeRoot } from '../../components/assets'
import {
  getProjectFileByFilePath,
  projectContentFile,
  transformContentsTree,
  walkContentsTree,
} from '../../components/assets'
import type { FileResult } from '../shared/file-utils'
import { extractAsset, extractImage, extractText } from '../shared/file-utils'
import { emptySet } from '../shared/set-utils'
import { fastForEach } from '../shared/utils'
import { foldEither, isRight, maybeEitherToMaybe } from '../shared/either'
import { memoize } from '../shared/memoize'
import { filenameFromParts, getFilenameParts } from '../../components/images'
import globToRegexp from 'glob-to-regexp'
import { is } from '../shared/equality-utils'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import json5 from 'json5'
import type { FilePathMappings, FilePathMapping } from '../workers/common/project-file-utils'
export type { FilePathMappings, FilePathMapping }

export const sceneMetadata = _sceneMetadata // This is a hotfix for a circular dependency AND a leaking of utopia-api into the workers

export function isUtopiaAPIComponent(elementName: JSXElementName, imports: Imports): boolean {
  const utopiaAPI = imports['utopia-api']
  if (utopiaAPI == null) {
    return false
  } else {
    if (PP.depth(elementName.propertyPath) === 0) {
      return pluck(utopiaAPI.importedFromWithin, 'name').includes(elementName.baseVariable)
    } else {
      return utopiaAPI.importedAs === elementName.baseVariable
    }
  }
}

export function isUtopiaAPIComponentFromMetadata(
  elementInstanceMetadata: ElementInstanceMetadata,
): boolean {
  const foundImportInfo = elementInstanceMetadata.importInfo
  if (foundImportInfo == null || isSameFileOrigin(foundImportInfo)) {
    return false
  } else {
    return foundImportInfo.filePath === 'utopia-api'
  }
}

export function isGivenUtopiaAPIElement(
  element: JSXElementChild,
  imports: Imports,
  componentName: string,
): boolean {
  return (
    isJSXElement(element) && isGivenUtopiaAPIElementFromName(element.name, imports, componentName)
  )
}

function isGivenUtopiaAPIElementFromName(
  jsxElementName: JSXElementName,
  imports: Imports,
  componentName: string,
): boolean {
  const utopiaAPI = imports['utopia-api']
  if (utopiaAPI == null) {
    return false
  } else {
    if (PP.depth(jsxElementName.propertyPath) === 0) {
      for (const fromWithin of utopiaAPI.importedFromWithin) {
        if (fromWithin.alias === jsxElementName.baseVariable && fromWithin.name === componentName) {
          return true
        }
      }
      return false
    } else {
      return (
        utopiaAPI.importedAs === jsxElementName.baseVariable &&
        PP.isSameProperty(jsxElementName.propertyPath, componentName)
      )
    }
  }
}

export function isGivenUtopiaElementFromMetadata(
  elementInstanceMetadata: ElementInstanceMetadata,
  componentName: string,
): boolean {
  const foundImportInfo = elementInstanceMetadata.importInfo
  if (foundImportInfo != null && isImportedOrigin(foundImportInfo)) {
    return (
      foundImportInfo.filePath === 'utopia-api' && foundImportInfo.exportedName === componentName
    )
  } else {
    return false
  }
}

export function isSceneAgainstImports(element: JSXElementChild, imports: Imports): boolean {
  return isGivenUtopiaAPIElement(element, imports, 'Scene')
}

export function isSceneFromMetadata(elementInstanceMetadata: ElementInstanceMetadata): boolean {
  return isGivenUtopiaElementFromMetadata(elementInstanceMetadata, 'Scene')
}

export function isRemixSceneAgainstImports(element: JSXElementChild, imports: Imports): boolean {
  return isGivenUtopiaAPIElement(element, imports, 'RemixScene')
}

export function isRemixOutletAgainstImports(element: JSXElementChild, imports: Imports): boolean {
  if (!isJSXElement(element)) {
    return false
  }

  const remix = imports['@remix-run/react']
  if (remix == null) {
    return false
  }

  for (const fromWithin of remix.importedFromWithin) {
    if (fromWithin.alias === element.name.baseVariable && fromWithin.name === 'Outlet') {
      return true
    }
  }

  return (
    remix.importedAs === element.name.baseVariable &&
    PP.isSameProperty(element.name.propertyPath, 'Outlet')
  )
}

export function isRemixSceneElement(
  element: JSXElementChild,
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): boolean {
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return isRemixSceneAgainstImports(element, file.fileContents.parsed.imports)
  } else {
    return false
  }
}

export function isRemixOutletElement(
  element: JSXElementChild,
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): boolean {
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return isRemixOutletAgainstImports(element, file.fileContents.parsed.imports)
  } else {
    return false
  }
}

export function isEllipseAgainstImports(jsxElementName: JSXElementName, imports: Imports): boolean {
  return isGivenUtopiaAPIElementFromName(jsxElementName, imports, 'Ellipse')
}

export function isRectangleAgainstImports(
  jsxElementName: JSXElementName,
  imports: Imports,
): boolean {
  return isGivenUtopiaAPIElementFromName(jsxElementName, imports, 'Rectangle')
}

export function isViewAgainstImports(jsxElementName: JSXElementName, imports: Imports): boolean {
  return isGivenUtopiaAPIElementFromName(jsxElementName, imports, 'View')
}

export function isViewLikeFromMetadata(elementInstanceMetadata: ElementInstanceMetadata): boolean {
  return (
    isGivenUtopiaElementFromMetadata(elementInstanceMetadata, 'View') ||
    isGivenUtopiaElementFromMetadata(elementInstanceMetadata, 'FlexRow') ||
    isGivenUtopiaElementFromMetadata(elementInstanceMetadata, 'FlexCol')
  )
}

export function isImg(jsxElementName: JSXElementName): boolean {
  return (
    PP.depth(jsxElementName.propertyPath) === 0 &&
    getJSXElementNameLastPart(jsxElementName) === 'img'
  )
}

export function isAnimatedElement(
  elementInstanceMetadata: ElementInstanceMetadata | null,
): boolean {
  const importInfo = elementInstanceMetadata?.importInfo
  if (importInfo != null && isImportedOrigin(importInfo)) {
    return importInfo.filePath === 'react-spring' && importInfo.exportedName === 'animated'
  } else {
    return false
  }
}

export function isHTMLComponent(jsxElementName: JSXElementName, imports: Imports): boolean {
  return (
    PP.depth(jsxElementName.propertyPath) === 0 &&
    isHTMLComponentFromBaseName(jsxElementName.baseVariable, imports)
  )
}

function isHTMLComponentFromBaseName(baseName: string, imports: Imports): boolean {
  const imported = Object.keys(imports).some((importKey) => {
    const fromImports = imports[importKey]
    return pluck(fromImports.importedFromWithin, 'name').includes(baseName)
  })
  if (imported) {
    return false
  } else {
    return intrinsicHTMLElementNamesAsStrings.includes(baseName)
  }
}

export function importInfoFromImportDetails(
  name: JSXElementName,
  imports: Imports,
  filePath: string,
): ImportInfo {
  const baseVariable = name.baseVariable

  const err = mapDropNulls((pathOrModuleName) => {
    const importDetail = imports[pathOrModuleName]
    const importAlias = importDetail.importedFromWithin.find(
      (fromWithin) => fromWithin.alias === baseVariable,
    )

    const absolutePath = absolutePathFromRelativePath(filePath, false, pathOrModuleName)

    if (importAlias != null) {
      return createImportedFrom(importAlias.alias, importAlias.name, absolutePath)
    } else if (importDetail.importedAs === baseVariable) {
      return createImportedFrom(importDetail.importedAs, null, absolutePath)
    } else if (importDetail.importedWithName === baseVariable) {
      return createImportedFrom(importDetail.importedWithName, null, absolutePath)
    } else {
      return null
    }
  }, Object.keys(imports))

  const foundImportDetail = err[0] ?? createNotImported(filePath, baseVariable)

  return foundImportDetail
}

export function isImportedComponentFromProjectFiles(
  element: ElementInstanceMetadata | null,
  filePathMappings: FilePathMappings,
): boolean {
  return !isImportedComponentNPM(element, filePathMappings)
}

export function isImportedComponent(
  elementInstanceMetadata: ElementInstanceMetadata | null,
  filePathMappings: FilePathMappings,
): boolean {
  const importInfo = elementInstanceMetadata?.importInfo
  if (importInfo != null && isImportedOrigin(importInfo)) {
    const importKey = importInfo.filePath
    const isMappedFilePath = filePathMappings.some(([re, _]) => {
      const result = re.test(importKey)
      re.lastIndex = 0 // Reset the regex!
      return result
    })
    return !isMappedFilePath && !importKey.startsWith('.') && !importKey.startsWith('/')
  } else {
    return false
  }
}

export function isIntrinsicElementMetadata(
  elementInstanceMetadata: ElementInstanceMetadata,
): boolean {
  return foldEither(
    isIntrinsicElementFromString,
    (child) => {
      if (isJSXElement(child)) {
        return isIntrinsicElement(child.name)
      } else {
        return false
      }
    },
    elementInstanceMetadata.element,
  )
}

export function isIntrinsicHTMLElementMetadata(
  elementInstanceMetadata: ElementInstanceMetadata,
): boolean {
  return foldEither(
    isIntrinsicHTMLElementString,
    (child) => {
      if (isJSXElement(child)) {
        return isIntrinsicHTMLElement(child.name)
      } else {
        return false
      }
    },
    elementInstanceMetadata.element,
  )
}

export function isImportedComponentNPM(
  elementInstanceMetadata: ElementInstanceMetadata | null,
  filePathMappings: FilePathMappings,
): boolean {
  return (
    (elementInstanceMetadata != null &&
      isIntrinsicElementMetadata(elementInstanceMetadata) &&
      !isIntrinsicHTMLElementMetadata(elementInstanceMetadata)) ||
    (isImportedComponent(elementInstanceMetadata, filePathMappings) &&
      elementInstanceMetadata != null &&
      !isUtopiaAPIComponentFromMetadata(elementInstanceMetadata))
  )
}

export function getOrDefaultScenes(parsedSuccess: ParseSuccess): UtopiaJSXComponent {
  const utopiaComponentFromTopLevelElements = fishOutUtopiaCanvasFromTopLevelElements(
    parsedSuccess.topLevelElements,
  )
  if (utopiaComponentFromTopLevelElements != null) {
    return utopiaComponentFromTopLevelElements
  }
  // If all fails, let's return an empty default component
  return EmptyUtopiaCanvasComponent
}

export function getComponentsFromTopLevelElements(
  topLevelElements: Array<TopLevelElement>,
): Array<UtopiaJSXComponent> {
  let utopiaJSXComponents: Array<UtopiaJSXComponent> = []
  Utils.fastForEach(topLevelElements, (topLevelElement) => {
    if (isUtopiaJSXComponent(topLevelElement)) {
      utopiaJSXComponents.push(topLevelElement)
    }
  })
  return utopiaJSXComponents
}

function getUtopiaJSXComponentsFromSuccessInner(success: ParseSuccess): Array<UtopiaJSXComponent> {
  return getComponentsFromTopLevelElements(success.topLevelElements)
}

export const getUtopiaJSXComponentsFromSuccess = memoize(getUtopiaJSXComponentsFromSuccessInner)

export function applyUtopiaJSXComponentsChanges(
  topLevelElements: Array<TopLevelElement>,
  newUtopiaComponents: Array<UtopiaJSXComponent>,
): Array<TopLevelElement> {
  // Run through the old top level elements, replacing the exported elements with those in the
  // newly updated result with the same name.
  // If it doesn't exist in the updated result, delete it.
  // For any new items in the updated result, add them in.
  const addedSoFar: Set<string | null> = emptySet()
  let newTopLevelElements: Array<TopLevelElement> = []
  fastForEach(topLevelElements, (oldTopLevelElement) => {
    if (isUtopiaJSXComponent(oldTopLevelElement)) {
      const updatedElement = newUtopiaComponents.find((e) => e.name === oldTopLevelElement.name)
      if (updatedElement !== undefined) {
        addedSoFar.add(updatedElement.name)
        newTopLevelElements.push(updatedElement)
      }
    } else {
      newTopLevelElements.push(oldTopLevelElement)
    }
  })

  fastForEach(newUtopiaComponents, (updatedElement) => {
    if (!addedSoFar.has(updatedElement.name)) {
      newTopLevelElements.push(updatedElement)
    }
  })

  return newTopLevelElements
}

export function getHighlightBoundsFromParseResult(
  result: ParsedTextFile,
): HighlightBoundsForUids | null {
  return foldParsedTextFile<HighlightBoundsForUids | null>(
    (_) => {
      return null
    },
    (success) => {
      return success.highlightBounds
    },
    (_) => {
      return null
    },
    result,
  )
}

function getHighlightBoundsForProjectImpl(
  allFiles: ProjectContentTreeRoot,
): HighlightBoundsWithFileForUids {
  let allHighlightBounds: HighlightBoundsWithFileForUids = {}
  walkContentsTree(allFiles, (fullPath: string, file: ProjectFile) => {
    if (isTextFile(file)) {
      forEachParseSuccess((parsedFile) => {
        const fileHighlightBounds = parsedFile.highlightBounds
        forEachValue((bounds, uid) => {
          allHighlightBounds[uid] = { ...bounds, filePath: fullPath }
        }, fileHighlightBounds)
      }, file.fileContents.parsed)
    }
  })

  return allHighlightBounds
}

export const getHighlightBoundsForProject = memoize(getHighlightBoundsForProjectImpl, {
  maxSize: 2,
  matchesArg: (a, b) => a === b,
})

export function updateParsedTextFileHighlightBounds(
  result: ParsedTextFile,
  highlightBounds: HighlightBoundsForUids | null,
): ParsedTextFile {
  return foldParsedTextFile<ParsedTextFile>(
    (failure) => failure,
    (success) => {
      return { ...success, highlightBounds: highlightBounds ?? {} }
    },
    (unparsedResult) => unparsedResult,
    result,
  )
}

export function canUpdateRevisionsState(
  updated: RevisionsStateType,
  existing: RevisionsStateType,
): boolean {
  switch (existing) {
    case RevisionsState.BothMatch:
      return true
    case RevisionsState.ParsedAhead:
      return updated === RevisionsState.ParsedAhead || updated === RevisionsState.BothMatch
    case RevisionsState.CodeAhead:
    case RevisionsState.CodeAheadButPleaseTellVSCodeAboutIt:
      return (
        updated === RevisionsState.CodeAhead ||
        updated === RevisionsState.BothMatch ||
        updated === RevisionsState.CodeAheadButPleaseTellVSCodeAboutIt
      )
    default:
      const _exhaustiveCheck: never = existing
      throw new Error(`Invalid revisions state ${existing}`)
  }
}

export function isOlderThan(maybeNew: ProjectFile, existing: ProjectFile | null): boolean {
  if (existing == null) {
    return false
  }

  return (
    isTextFile(maybeNew) && isTextFile(existing) && maybeNew.versionNumber < existing.versionNumber
  )
}

export function updateUiJsCode(file: TextFile, code: string, codeIsNowAhead: boolean): TextFile {
  const revisionsState = codeIsNowAhead ? RevisionsState.CodeAhead : RevisionsState.BothMatch
  const fileContents: TextFileContents = {
    ...file.fileContents,
    revisionsState: revisionsState,
    code: code,
  }

  return textFile(
    fileContents,
    file.lastSavedContents,
    file.lastParseSuccess,
    file.versionNumber + 1,
  )
}

export function sameTextFile(first: ProjectFile, second: ProjectFile): boolean {
  if (isTextFile(first) && isTextFile(second)) {
    return first.fileContents === second.fileContents
  } else {
    return false
  }
}

// A layer over the mime-types library which means we can shim in things we need.
// Keep this in sync with Utopia/Web/Assets.hs.
export function mimeTypeLookup(filename: string): string | false {
  if (filename.endsWith('.ts')) {
    return 'application/x-typescript'
  } else if (filename.endsWith('.tsx')) {
    return 'application/x-tsx'
  } else if (filename.endsWith('.jsx')) {
    return 'application/x-jsx'
  } else {
    return MimeTypes.lookup(filename)
  }
}

type ProjectFileTypeExcludingDirectory = Exclude<ProjectFileType, 'DIRECTORY'>

export function fileTypeFromFileName(filename: null): null
export function fileTypeFromFileName(filename: string): ProjectFileTypeExcludingDirectory
export function fileTypeFromFileName(
  filename: string | null,
): ProjectFileTypeExcludingDirectory | null
export function fileTypeFromFileName(
  filename: string | null,
): ProjectFileTypeExcludingDirectory | null {
  if (filename == null) {
    return null
  }
  if (filename.endsWith('.svg')) {
    return 'ASSET_FILE'
  }
  if (isText(filename)) {
    return 'TEXT_FILE'
  } else {
    const mimeType = mimeTypeLookup(filename)
    if (mimeType === false) {
      return 'TEXT_FILE' // FIXME This is definitely not a safe assumption
    } else if (mimeType.startsWith('image/')) {
      return 'IMAGE_FILE'
    } else {
      return 'ASSET_FILE'
    }
  }
}

export function extractFile(file: File): Promise<FileResult> {
  const fileType = fileTypeFromFileName(file.name)
  switch (fileType) {
    case 'TEXT_FILE':
      return extractText(file)
    case 'IMAGE_FILE':
      return extractImage(file)
    case 'ASSET_FILE':
      return extractAsset(file)
  }
}

export function switchToFileType(from: ProjectFile, to: ProjectFileType): ProjectFile | null {
  switch (from.type) {
    case 'TEXT_FILE':
      switch (to) {
        case 'TEXT_FILE':
          return from
        case 'IMAGE_FILE':
        case 'DIRECTORY':
        case 'ASSET_FILE':
          return null
        default:
          const _exhaustiveCheck: never = to
          throw new Error(`Unhandled target type ${JSON.stringify(to)}.`)
      }
    case 'IMAGE_FILE':
      switch (to) {
        case 'IMAGE_FILE':
          return from
        case 'TEXT_FILE':
        case 'DIRECTORY':
        case 'ASSET_FILE':
          return null
        default:
          const _exhaustiveCheck: never = to
          throw new Error(`Unhandled target type ${JSON.stringify(to)}.`)
      }
    case 'DIRECTORY':
      switch (to) {
        case 'DIRECTORY':
          return from
        case 'TEXT_FILE':
        case 'IMAGE_FILE':
        case 'ASSET_FILE':
          return null
        default:
          const _exhaustiveCheck: never = to
          throw new Error(`Unhandled target type ${JSON.stringify(to)}.`)
      }
    case 'ASSET_FILE':
      switch (to) {
        case 'ASSET_FILE':
          return from
        case 'TEXT_FILE':
        case 'IMAGE_FILE':
        case 'DIRECTORY':
          return null
        default:
          const _exhaustiveCheck: never = to
          throw new Error(`Unhandled target type ${JSON.stringify(to)}.`)
      }
    default:
      const _exhaustiveCheck: never = from
      throw new Error(`Unhandled source type ${JSON.stringify(from)}.`)
  }
}

export function uniqueProjectContentID(
  filename: string,
  projectContents: ProjectContentTreeRoot,
): string {
  const startingIDCorrected = correctProjectContentsPath(filename)
  const fileWithSameNameExistsAlready =
    getProjectFileByFilePath(projectContents, startingIDCorrected) != null

  if (!fileWithSameNameExistsAlready) {
    return startingIDCorrected
  }

  const parts = getFilenameParts(startingIDCorrected)

  const makeNameWithCounter =
    parts !== null
      ? (counter: number) => filenameFromParts({ ...parts, deduplicationSeqNumber: counter })
      : (counter: number) => `${startingIDCorrected}_${counter}`

  let counter = 2
  // eslint-disable-next-line no-constant-condition
  while (true) {
    const possibleNewID = makeNameWithCounter(counter)
    if (getProjectFileByFilePath(projectContents, possibleNewID) != null) {
      counter += 1
    } else {
      return correctProjectContentsPath(possibleNewID)
    }
  }
}

export function fileExists(projectContents: ProjectContentTreeRoot, filename: string): boolean {
  const filenameCorrected = correctProjectContentsPath(filename)
  return getProjectFileByFilePath(projectContents, filenameCorrected) != null
}

export function saveTextFileContents(
  file: TextFile,
  contents: TextFileContents,
  manualSave: boolean,
): TextFile {
  const savedContent = updateLastSavedContents(
    file.lastSavedContents,
    file.fileContents,
    manualSave,
  )
  const lastParseSuccess = isParseSuccess(contents.parsed) ? contents.parsed : file.lastParseSuccess
  return textFile(contents, savedContent, lastParseSuccess, file.versionNumber + 1)
}

export function updateLastSavedContents<T>(
  lastSavedContents: T | null,
  contents: T,
  manualSave: boolean,
): T | null {
  if (manualSave) {
    return null
  }
  if (lastSavedContents == null) {
    return contents
  }
  return lastSavedContents
}

export function isModifiedFile(file: ProjectFile): boolean {
  return isTextFile(file) && file.lastSavedContents != null
}

export function revertFile(file: ProjectFile): ProjectFile {
  if (isModifiedFile(file)) {
    switch (file.type) {
      case 'TEXT_FILE':
        return {
          ...file,
          fileContents: file.lastSavedContents,
          lastSavedContents: null,
        } as TextFile
      default:
        throw new Error(`Only text files can be modified.`)
    }
  }
  return file
}

export function saveFile(file: ProjectFile): ProjectFile {
  if (isModifiedFile(file)) {
    switch (file.type) {
      case 'TEXT_FILE':
        return saveTextFileContents(file, file.fileContents, true)
      default:
        throw new Error(`Only text files can be modified.`)
    }
  }
  return file
}

export function correctProjectContentsPath(path: string): string {
  const parsed = pathParse(path)
  if (parsed.dir === '/' || parsed.dir === '') {
    if (parsed.base.startsWith('/')) {
      return parsed.base
    } else {
      return `/${parsed.base}`
    }
  } else {
    if (parsed.dir.startsWith('/')) {
      return `${parsed.dir}/${parsed.base}`
    } else {
      return `/${parsed.dir}/${parsed.base}`
    }
  }
}

export function applyToAllUIJSFiles(
  allFiles: ProjectContentTreeRoot,
  fn: (filename: string, uiJSFile: TextFile) => TextFile,
): ProjectContentTreeRoot {
  return transformContentsTree(allFiles, (tree: ProjectContentsTree) => {
    if (tree.type === 'PROJECT_CONTENT_FILE') {
      if (isTextFile(tree.content)) {
        const updatedContent = fn(tree.fullPath, tree.content)
        return projectContentFile(tree.fullPath, updatedContent)
      } else {
        return tree
      }
    } else {
      return tree
    }
  })
}

export function updateFileContents(
  contents: string,
  file: ProjectFile,
  manualSave: boolean,
): ProjectFile {
  switch (file.type) {
    case 'DIRECTORY':
    case 'IMAGE_FILE':
    case 'ASSET_FILE':
      return file
    case 'TEXT_FILE':
      const uiJsLastSavedContents = updateLastSavedContents(
        file.fileContents,
        file.lastSavedContents,
        manualSave,
      )

      const newParsed = updateParsedTextFileHighlightBounds(
        file.fileContents.parsed,
        getHighlightBoundsFromParseResult(file.fileContents.parsed), // here we just update the code without updating the highlights!
      )
      const newContents = textFileContents(contents, newParsed, RevisionsState.CodeAhead)
      return textFile(
        newContents,
        uiJsLastSavedContents,
        file.lastParseSuccess,
        file.versionNumber + 1,
      )
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

export function getSavedCodeFromTextFile(file: TextFile): string {
  return file.lastSavedContents?.code ?? file.fileContents.code
}

export function getUnsavedCodeFromTextFile(file: TextFile): string | null {
  return file.lastSavedContents == null ? null : file.fileContents.code
}

export function getDefaultExportedTopLevelElement(file: TextFile): JSXElementChild | null {
  if (file.fileContents.parsed.type !== 'PARSE_SUCCESS') {
    return null
  }

  const defaultExportName =
    file.fileContents.parsed.exportsDetail.find(isExportDefaultFunctionOrClass)?.name ?? null

  if (defaultExportName == null) {
    return null
  }

  return (
    file.fileContents.parsed.topLevelElements.find(
      (t): t is UtopiaJSXComponent =>
        t.type === 'UTOPIA_JSX_COMPONENT' && t.name === defaultExportName,
    )?.rootElement ?? null
  )
}

export function getDefaultExportNameAndUidFromFile(
  projectContents: ProjectContentTreeRoot,
  filePath: string,
): { name: string; uid: string | null } | null {
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (
    file == null ||
    file.type != 'TEXT_FILE' ||
    file.fileContents.parsed.type !== 'PARSE_SUCCESS'
  ) {
    return null
  }

  const defaultExportName =
    file.fileContents.parsed.exportsDetail.find(isExportDefault)?.name ?? null

  if (defaultExportName == null) {
    return null
  }

  const elementUid =
    file.fileContents.parsed.topLevelElements.find(
      (t): t is UtopiaJSXComponent =>
        t.type === 'UTOPIA_JSX_COMPONENT' && t.name === defaultExportName,
    )?.rootElement.uid ?? null

  return { name: defaultExportName, uid: elementUid }
}

export function fileExportsFunctionWithName(
  projectContents: ProjectContentTreeRoot,
  filePath: string,
  componentName: string,
): boolean {
  const file = getProjectFileByFilePath(projectContents, filePath)
  if (
    file == null ||
    file.type != 'TEXT_FILE' ||
    file.fileContents.parsed.type !== 'PARSE_SUCCESS'
  ) {
    return false
  }

  return file.fileContents.parsed.exportsDetail.some(
    (v) => isExportFunction(v) && v.functionName === componentName,
  )
}

export function getTopLevelElementByExportsDetail(
  file: ParseSuccess,
  nameToLookFor: string,
): UtopiaJSXComponent | null {
  return (
    file.topLevelElements.find(
      (t): t is UtopiaJSXComponent => t.type === 'UTOPIA_JSX_COMPONENT' && t.name === nameToLookFor,
    ) ?? null
  )
}

export const getFilePathMappings = memoize(getFilePathMappingsImpl, { maxSize: 1, matchesArg: is })

function getFilePathMappingsImpl(projectContents: ProjectContentTreeRoot): FilePathMappings {
  const jsConfigFile = getProjectFileByFilePath(projectContents, 'jsconfig.json')
  if (jsConfigFile != null && isTextFile(jsConfigFile)) {
    return getFilePathMappingsFromConfigFile(jsConfigFile)
  }
  const tsConfigFile = getProjectFileByFilePath(projectContents, 'tsconfig.json')
  if (tsConfigFile != null && isTextFile(tsConfigFile)) {
    return getFilePathMappingsFromConfigFile(tsConfigFile)
  }

  return []
}

const getFilePathMappingsFromConfigFile = memoize(getFilePathMappingsFromConfigFileImpl, {
  maxSize: 1,
  matchesArg: is,
})

function getFilePathMappingsFromConfigFileImpl(configFile: TextFile): FilePathMappings {
  try {
    const parsedJSON = json5.parse(configFile.fileContents.code)
    if (typeof parsedJSON === 'object') {
      const compilerOptions = propOrNull('compilerOptions', parsedJSON)
      const paths = propOrNull('paths', compilerOptions)
      if (paths != null && typeof paths === 'object' && !Array.isArray(paths)) {
        // The file path mappings are using glob patterns, and are a mapping from a
        // pattern, to an array of replacements: https://code.visualstudio.com/docs/languages/jsconfig#_using-webpack-aliases
        const pathEntries = Object.entries(paths)
        const pathMappings = mapDropNulls(([k, v]) => {
          if (Array.isArray(v)) {
            const values = mapDropNulls((s) => {
              if (typeof s === 'string') {
                // FIXME These should be relative to the `baseUrl`
                return `/${replaceAllStarsWithIndexedGroups(s)}`
              } else {
                return null
              }
            }, v)

            const globRegex = globToRegexp(k, { flags: 'g', globstar: true })
            const stickyRegex = new RegExp(globRegex, 'y')
            ;(stickyRegex as any).skipDeepFreeze = true

            return [stickyRegex, values] as FilePathMapping
          } else {
            return null
          }
        }, pathEntries)

        return pathMappings
      }
    }
  } catch (e) {
    console.error('Error parsing file path mappings.', e)
  }

  return []
}

// This horrorshow function is for turning glob patterns into suitable replacement strings,
// e.g. 'app/**/*.js' will become 'app/$1$2', which can then be used in a string replacement
// used by the file mappings containing glob patterns
function replaceAllStarsWithIndexedGroups(s: string) {
  let index = 1
  return s.replace(/\*+\/?/g, () => `$${index++}`)
}
