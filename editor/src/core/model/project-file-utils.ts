import * as MimeTypes from 'mime-types'
import pathParse from 'path-parse'
import * as PP from '../shared/property-path'
import { isText } from 'istextorbinary'
import { intrinsicHTMLElementNamesAsStrings } from '../shared/dom-utils'
import Utils from '../../utils/utils'
import {
  Directory,
  HighlightBoundsForUids,
  ImageFile,
  Imports,
  ParsedTextFile,
  ParseSuccess,
  ProjectFile,
  ProjectFileType,
  RevisionsState,
  TextFile,
  AssetFile,
  foldParsedTextFile,
  isTextFile,
  textFile,
  TextFileContents,
  textFileContents,
  HighlightBoundsWithFileForUids,
  forEachParseSuccess,
  isParseSuccess,
  RevisionsStateType,
} from '../shared/project-file-types'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  JSXElementChild,
  JSXElementName,
  TopLevelElement,
  UtopiaJSXComponent,
  getJSXElementNameLastPart,
  ImportInfo,
  createImportedFrom,
  createNotImported,
  ElementInstanceMetadata,
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
import { forEachValue } from '../shared/object-utils'
import {
  getContentsTreeFileFromString,
  projectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  transformContentsTree,
  walkContentsTree,
} from '../../components/assets'
import { extractAsset, extractImage, extractText, FileResult } from '../shared/file-utils'
import { emptySet } from '../shared/set-utils'
import { fastForEach } from '../shared/utils'
import { foldEither, isRight, maybeEitherToMaybe } from '../shared/either'
import { memoize } from '../shared/memoize'
import { filenameFromParts, getFilenameParts } from '../../components/images'

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

    if (importAlias != null) {
      return createImportedFrom(importAlias.alias, importAlias.name, pathOrModuleName)
    } else if (importDetail.importedAs === baseVariable) {
      return createImportedFrom(importDetail.importedAs, null, pathOrModuleName)
    } else if (importDetail.importedWithName === baseVariable) {
      return createImportedFrom(importDetail.importedWithName, null, pathOrModuleName)
    } else {
      return null
    }
  }, Object.keys(imports))

  const foundImportDetail = err[0] ?? createNotImported(filePath, baseVariable)

  return foundImportDetail
}

export function getFilePathForImportedComponent(
  element: ElementInstanceMetadata | null,
): string | null {
  const importInfo = element?.importInfo
  if (importInfo != null && isImportedOrigin(importInfo)) {
    return importInfo.filePath
  } else {
    return null
  }
}

export function isImportedComponentFromProjectFiles(
  element: ElementInstanceMetadata | null,
): boolean {
  return !isImportedComponentNPM(element)
}

export function isImportedComponent(
  elementInstanceMetadata: ElementInstanceMetadata | null,
): boolean {
  const importInfo = elementInstanceMetadata?.importInfo
  if (importInfo != null && isImportedOrigin(importInfo)) {
    const importKey = importInfo.filePath
    return !importKey.startsWith('.') && !importKey.startsWith('/')
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
): boolean {
  return (
    (elementInstanceMetadata != null &&
      isIntrinsicElementMetadata(elementInstanceMetadata) &&
      !isIntrinsicHTMLElementMetadata(elementInstanceMetadata)) ||
    (isImportedComponent(elementInstanceMetadata) &&
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

export const getUtopiaJSXComponentsFromSuccess = Utils.memoize(
  getUtopiaJSXComponentsFromSuccessInner,
)

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
  equals: (a, b) => a === b,
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
      return updated === RevisionsState.CodeAhead || updated === RevisionsState.BothMatch
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

export function updateFileIfPossible(
  updated: ProjectFile,
  existing: ProjectFile | null,
): ProjectFile | 'cant-update' {
  if (existing == null || !isTextFile(existing)) {
    return updated
  }

  if (
    isTextFile(updated) &&
    isOlderThan(existing, updated) &&
    canUpdateRevisionsState(
      updated.fileContents.revisionsState,
      existing.fileContents.revisionsState,
    )
  ) {
    return updated
  }

  return 'cant-update'
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

export function imageFile(
  imageType: string | undefined,
  base64: string | undefined,
  width: number | undefined,
  height: number | undefined,
  hash: number,
): ImageFile {
  return {
    type: 'IMAGE_FILE',
    imageType: imageType,
    base64: base64,
    width: width,
    height: height,
    hash: hash,
  }
}

export function assetFile(base64: string | undefined): AssetFile {
  return {
    type: 'ASSET_FILE',
    base64: base64,
  }
}

export function directory(): Directory {
  return {
    type: 'DIRECTORY',
  }
}

export function sameTextFile(first: ProjectFile, second: ProjectFile): boolean {
  if (isTextFile(first) && isTextFile(second)) {
    return first.fileContents === second.fileContents
  } else {
    return false
  }
}

export function isImageFile(projectFile: ProjectFile): projectFile is ImageFile {
  return projectFile.type === 'IMAGE_FILE'
}

export function isDirectory(projectFile: ProjectFile): projectFile is Directory {
  return projectFile.type === 'DIRECTORY'
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
    getContentsTreeFileFromString(projectContents, startingIDCorrected) != null

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
    if (getContentsTreeFileFromString(projectContents, possibleNewID) != null) {
      counter += 1
    } else {
      return correctProjectContentsPath(possibleNewID)
    }
  }
}

export function fileExists(projectContents: ProjectContentTreeRoot, filename: string): boolean {
  const filenameCorrected = correctProjectContentsPath(filename)
  return getContentsTreeFileFromString(projectContents, filenameCorrected) != null
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
