import * as MimeTypes from 'mime-types'
import * as pathParse from 'path-parse'
import * as PP from '../shared/property-path'
import { isText } from 'istextorbinary'
import { intrinsicHTMLElementNamesAsStrings } from '../shared/dom-utils'
import Utils from '../../utils/utils'
import {
  Directory,
  HighlightBoundsForUids,
  ImageFile,
  Imports,
  isParseFailure,
  ParsedTextFile,
  ParseSuccess,
  ProjectContents,
  ProjectFile,
  ProjectFileType,
  RevisionsState,
  SceneMetadata,
  TextFile,
  AssetFile,
  foldParsedTextFile,
  isTextFile,
  textFile,
  TextFileContents,
  textFileContents,
  unparsed,
  HighlightBoundsWithFileForUids,
  forEachParseSuccess,
} from '../shared/project-file-types'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  JSXElementChild,
  JSXElementName,
  TopLevelElement,
  UtopiaJSXComponent,
  getJSXElementNameLastPart,
  jsxElementNameEquals,
  ImportInfo,
  createImportedFrom,
  createNotImported,
  ElementInstanceMetadata,
} from '../shared/element-template'
import {
  sceneMetadata as _sceneMetadata,
  fishOutUtopiaCanvasFromTopLevelElements,
  EmptyUtopiaCanvasComponent,
} from './scene-utils'
import { mapDropNulls, pluck } from '../shared/array-utils'
import { forEachValue, mapValues } from '../shared/object-utils'
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
import { foldEither, isLeft, isRight, maybeEitherToMaybe } from '../shared/either'
import { splitAt } from '../shared/string-utils'

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
  const foundImportInfo = maybeEitherToMaybe(elementInstanceMetadata.importInfo)
  if (foundImportInfo != null) {
    return foundImportInfo.path === 'utopia-api'
  } else {
    return false
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
      return (
        pluck(utopiaAPI.importedFromWithin, 'name').includes(jsxElementName.baseVariable) &&
        jsxElementName.baseVariable === componentName
      )
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
  const foundImportInfo = maybeEitherToMaybe(elementInstanceMetadata.importInfo)
  if (foundImportInfo != null) {
    return foundImportInfo.path === 'utopia-api' && foundImportInfo.originalName === componentName
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

export function isUtopiaAPITextElement(element: JSXElementChild, imports: Imports): boolean {
  return isJSXElement(element) && isTextAgainstImports(element.name, imports)
}

export function isUtopiaAPITextElementFromMetadata(
  elementInstanceMetadata: ElementInstanceMetadata,
): boolean {
  return isGivenUtopiaElementFromMetadata(elementInstanceMetadata, 'Text')
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

export function isViewFromMetadata(elementInstanceMetadata: ElementInstanceMetadata): boolean {
  return isGivenUtopiaElementFromMetadata(elementInstanceMetadata, 'View')
}

export function isTextAgainstImports(jsxElementName: JSXElementName, imports: Imports): boolean {
  return isGivenUtopiaAPIElementFromName(jsxElementName, imports, 'Text')
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
  if (importInfo != null && isRight(importInfo)) {
    return importInfo.value.path === 'react-spring' && importInfo.value.originalName === 'animated'
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

export function importInfoFromImportDetails(name: JSXElementName, imports: Imports): ImportInfo {
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

  const foundImportDetail = err[0] ?? createNotImported()

  return foundImportDetail
}

export function getFilePathForImportedComponent(
  element: ElementInstanceMetadata | null,
): string | null {
  const importInfo = element?.importInfo
  if (importInfo != null && isRight(importInfo)) {
    return importInfo.value.path
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
  if (importInfo != null && isRight(importInfo)) {
    const importKey = importInfo.value.path
    return !importKey.startsWith('.') && !importKey.startsWith('/')
  } else {
    return false
  }
}

export function isImportedComponentNPM(
  elementInstanceMetadata: ElementInstanceMetadata | null,
): boolean {
  return (
    isImportedComponent(elementInstanceMetadata) &&
    elementInstanceMetadata != null &&
    !isUtopiaAPIComponentFromMetadata(elementInstanceMetadata)
  )
}

const defaultEmptyUtopiaComponent = EmptyUtopiaCanvasComponent

export function getOrDefaultScenes(parsedSuccess: ParseSuccess): UtopiaJSXComponent {
  const utopiaComponentFromTopLevelElements = fishOutUtopiaCanvasFromTopLevelElements(
    parsedSuccess.topLevelElements,
  )
  if (utopiaComponentFromTopLevelElements != null) {
    return utopiaComponentFromTopLevelElements
  }
  // If all fails, let's return an empty default component
  return defaultEmptyUtopiaComponent
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
  const addedSoFar: Set<string> = emptySet()
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

export function getHighlightBoundsForProject(
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
  updated: RevisionsState,
  existing: RevisionsState,
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
    isTextFile(maybeNew) &&
    isTextFile(existing) &&
    maybeNew.lastRevisedTime < existing.lastRevisedTime
  )
}

export function canUpdateFile(updated: ProjectFile, existing: ProjectFile | null): boolean {
  if (existing == null) {
    return true
  }

  if (isTextFile(existing)) {
    return (
      isTextFile(updated) &&
      isTextFile(existing) &&
      isOlderThan(existing, updated) &&
      canUpdateRevisionsState(
        updated.fileContents.revisionsState,
        existing.fileContents.revisionsState,
      )
    )
  }

  return true
}

export function updateUiJsCode(file: TextFile, code: string, codeIsNowAhead: boolean): TextFile {
  const revisionsState = codeIsNowAhead ? RevisionsState.CodeAhead : RevisionsState.BothMatch
  const fileContents: TextFileContents = {
    ...file.fileContents,
    revisionsState: revisionsState,
    code: code,
  }

  return textFile(fileContents, file.lastSavedContents, Date.now())
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

export function assetFile(): AssetFile {
  return {
    type: 'ASSET_FILE',
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
  startingID: string,
  projectContents: ProjectContentTreeRoot,
): string {
  const startingIDCorrected = correctProjectContentsPath(startingID)
  if (getContentsTreeFileFromString(projectContents, startingIDCorrected) != null) {
    const firstIndexOfFullStop = startingIDCorrected.indexOf('.')
    if (firstIndexOfFullStop === -1) {
      let counter = 2
      // eslint-disable-next-line no-constant-condition
      while (true) {
        const possibleNewID = `${startingIDCorrected}_${counter}`
        if (getContentsTreeFileFromString(projectContents, possibleNewID) != null) {
          counter += 1
        } else {
          return correctProjectContentsPath(possibleNewID)
        }
      }
    } else {
      // Kinda assume it's a filename.
      const [prefix, suffixWithFullStop] = splitAt(firstIndexOfFullStop, startingIDCorrected)
      const suffix = suffixWithFullStop.slice(1)
      let counter = 2
      // eslint-disable-next-line no-constant-condition
      while (true) {
        const possibleNewID = `${prefix}_${counter}.${suffix}`
        if (getContentsTreeFileFromString(projectContents, possibleNewID) != null) {
          counter += 1
        } else {
          return correctProjectContentsPath(possibleNewID)
        }
      }
    }
  } else {
    return startingIDCorrected
  }
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
  const contentsUpdated = contents !== file.fileContents
  return textFile(contents, savedContent, contentsUpdated ? Date.now() : file.lastRevisedTime)
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
      return textFile(newContents, uiJsLastSavedContents, Date.now())
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
