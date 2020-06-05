import * as MimeTypes from 'mime-types'
import * as pathParse from 'path-parse'
import * as R from 'ramda'
import { NormalisedFrame } from 'utopia-api'
import * as PP from '../shared/property-path'
import { intrinsicHTMLElementNamesAsStrings } from '../shared/dom-utils'
import Utils from '../../utils/utils'
import { bimapEither, foldEither, mapEither } from '../shared/either'
import {
  CanvasMetadata,
  CanvasMetadataParseResult,
  CodeFile,
  Directory,
  ElementCanvasMetadata,
  HighlightBoundsForUids,
  ImageFile,
  Imports,
  isCodeFile,
  isParseFailure,
  isUIJSFile,
  ParseResult,
  ParseSuccess,
  ProjectContents,
  ProjectFile,
  ProjectFileType,
  RevisionsState,
  SceneContainer,
  SceneMetadata,
  UIJSFile,
  AssetFile,
} from '../shared/project-file-types'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  JSXElementChild,
  JSXElementName,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../shared/element-template'
import {
  sceneMetadata as _sceneMetadata,
  fishOutUtopiaCanvasFromTopLevelElements,
  EmptyUtopiaCanvasComponent,
} from './scene-utils'
import { pluck } from '../shared/array-utils'

export function emptyElementCanvasMetadata(): ElementCanvasMetadata {
  return {}
}

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

export function isGivenUtopiaAPIElement(
  element: JSXElementChild,
  imports: Imports,
  elementName: string,
): boolean {
  const utopiaAPI = imports['utopia-api']
  if (utopiaAPI == null) {
    return false
  } else {
    if (isJSXElement(element)) {
      const elemName = element.name
      if (PP.depth(elemName.propertyPath) === 0) {
        return (
          pluck(utopiaAPI.importedFromWithin, 'name').includes(elemName.baseVariable) &&
          elemName.baseVariable === elementName
        )
      } else {
        return (
          utopiaAPI.importedAs === elemName.baseVariable &&
          PP.isSameProperty(elemName.propertyPath, elementName)
        )
      }
    } else {
      return false
    }
  }
}

export function isUtopiaAPITextElement(element: JSXElementChild, imports: Imports): boolean {
  return isGivenUtopiaAPIElement(element, imports, 'Text')
}

export function isHTMLComponent(elementName: JSXElementName, imports: Imports): boolean {
  if (PP.depth(elementName.propertyPath) === 0) {
    const imported = Object.keys(imports).some((importKey) => {
      const fromImports = imports[importKey]
      return pluck(fromImports.importedFromWithin, 'name').includes(elementName.baseVariable)
    })
    if (imported) {
      return false
    } else {
      return intrinsicHTMLElementNamesAsStrings.includes(elementName.baseVariable)
    }
  } else {
    return false
  }
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

export function updateCanvasMetadataParseResult(
  transform: (metadata: CanvasMetadata) => CanvasMetadata,
  parseResult: CanvasMetadataParseResult,
): CanvasMetadataParseResult {
  return mapEither(transform, parseResult)
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

export function getHighlightBoundsFromParseResult(result: ParseResult): HighlightBoundsForUids {
  if (isParseFailure(result)) {
    return {}
  } else {
    return result.value.highlightBounds
  }
}

export function updateParseResultCode(
  result: ParseResult,
  code: string,
  highlightBounds: HighlightBoundsForUids,
): ParseResult {
  return bimapEither(
    (l) => {
      return { ...l, code: code }
    },
    (r) => {
      return { ...r, code: code, highlightBounds: highlightBounds }
    },
    result,
  )
}

export function uiJsFile(
  fileContents: ParseResult,
  lastSavedContents: ParseResult | null,
  revisionsState: RevisionsState,
  lastRevisedTime: number,
): UIJSFile {
  return {
    type: 'UI_JS_FILE',
    fileContents: fileContents,
    lastSavedContents: lastSavedContents,
    revisionsState: revisionsState,
    lastRevisedTime: lastRevisedTime,
  }
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
    isUIJSFile(maybeNew) &&
    isUIJSFile(existing) &&
    maybeNew.lastRevisedTime < existing.lastRevisedTime
  )
}

export function canUpdateFile(updated: ProjectFile, existing: ProjectFile | null): boolean {
  if (existing == null) {
    return true
  }

  if (isUIJSFile(existing)) {
    return (
      isUIJSFile(updated) &&
      isOlderThan(existing, updated) &&
      canUpdateRevisionsState(updated.revisionsState, existing.revisionsState)
    )
  }

  return true
}

export function updateUiJsCode(file: UIJSFile, code: string, codeIsNowAhead: boolean): UIJSFile {
  const updatedContents: ParseResult = bimapEither(
    (failure) => {
      return { ...failure, code: code }
    },
    (success) => {
      return { ...success, code: code }
    },
    file.fileContents,
  )

  const revisionsState = codeIsNowAhead ? RevisionsState.CodeAhead : RevisionsState.BothMatch

  return uiJsFile(updatedContents, file.lastSavedContents, revisionsState, Date.now())
}

export function codeFile(fileContents: string, lastSavedContents: string | null): CodeFile {
  return {
    type: 'CODE_FILE',
    fileContents: fileContents,
    lastSavedContents: lastSavedContents,
  }
}

export function imageFile(
  imageType: string | undefined,
  base64: string | undefined,
  width: number | undefined,
  height: number | undefined,
  hash: string,
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

export function sameCodeFile(first: ProjectFile, second: ProjectFile): boolean {
  if (isCodeFile(first) && isCodeFile(second)) {
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

export function fileTypeFromFileName(filename: string | null): ProjectFileType | null {
  if (filename == null) {
    return null
  }
  if (filename.endsWith('.js')) {
    return 'UI_JS_FILE'
  } else {
    const mimeType = mimeTypeLookup(filename)
    if (mimeType === false) {
      return null
    } else {
      if (mimeType.startsWith('image/')) {
        return 'IMAGE_FILE'
      } else if (mimeType.startsWith('application/') || mimeType.startsWith('text/')) {
        // Note for future us: The 'application/' prefix is possibly a bit wide.
        return 'CODE_FILE'
      } else {
        return 'ASSET_FILE'
      }
    }
  }
}

export function switchToFileType(from: ProjectFile, to: ProjectFileType): ProjectFile | null {
  switch (from.type) {
    case 'CODE_FILE':
      switch (to) {
        case 'CODE_FILE':
        case 'UI_JS_FILE':
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
        case 'UI_JS_FILE':
        case 'CODE_FILE':
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
        case 'UI_JS_FILE':
        case 'CODE_FILE':
        case 'IMAGE_FILE':
        case 'ASSET_FILE':
          return null
        default:
          const _exhaustiveCheck: never = to
          throw new Error(`Unhandled target type ${JSON.stringify(to)}.`)
      }
    case 'UI_JS_FILE':
      switch (to) {
        case 'UI_JS_FILE':
        case 'CODE_FILE':
          return from
        case 'IMAGE_FILE':
        case 'DIRECTORY':
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
        case 'CODE_FILE':
        case 'IMAGE_FILE':
        case 'DIRECTORY':
        case 'UI_JS_FILE':
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
  projectContents: ProjectContents,
): string {
  const startingIDCorrected = correctProjectContentsPath(startingID)
  if (startingIDCorrected in projectContents) {
    const firstIndexOfFullStop = startingIDCorrected.indexOf('.')
    if (firstIndexOfFullStop === -1) {
      let counter = 2
      // eslint-disable-next-line no-constant-condition
      while (true) {
        const possibleNewID = `${startingIDCorrected}_${counter}`
        if (possibleNewID in projectContents) {
          counter += 1
        } else {
          return correctProjectContentsPath(possibleNewID)
        }
      }
    } else {
      // Kinda assume it's a filename.
      const [prefix, suffixWithFullStop] = R.splitAt(firstIndexOfFullStop, startingIDCorrected)
      const suffix = R.tail(suffixWithFullStop)
      let counter = 2
      // eslint-disable-next-line no-constant-condition
      while (true) {
        const possibleNewID = `${prefix}_${counter}.${suffix}`
        if (possibleNewID in projectContents) {
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

export function getCodeFile(projectContents: ProjectContents, contentId: string): CodeFile | null {
  const projectContent = projectContents[contentId]
  if (projectContent == null) {
    return null
  } else {
    if (isCodeFile(projectContent)) {
      return projectContent
    } else {
      return null
    }
  }
}

export function saveCodeFileContents(file: CodeFile, contents: string, manualSave: boolean) {
  const savedContent = updateLastSavedContents(
    file.lastSavedContents,
    file.fileContents,
    manualSave,
  )
  return codeFile(contents, savedContent)
}

export function saveUIJSFileContents(
  file: UIJSFile,
  contents: ParseResult,
  manualSave: boolean,
  revisionsState: RevisionsState,
) {
  const savedContent = updateLastSavedContents(
    file.lastSavedContents,
    file.fileContents,
    manualSave,
  )

  const contentsUpdated = contents !== file.fileContents
  return uiJsFile(
    contents,
    savedContent,
    revisionsState,
    contentsUpdated ? Date.now() : file.lastRevisedTime,
  )
}

export function updateLastSavedContents<T>(
  lastSavedContents: T | null,
  contents: T,
  manualSave: boolean,
) {
  if (manualSave) {
    return null
  }
  if (lastSavedContents == null) {
    return contents
  }
  return lastSavedContents
}

export function isModifiedFile(file: ProjectFile) {
  return (isCodeFile(file) || isUIJSFile(file)) && file.lastSavedContents != null
}

export function revertFile(file: ProjectFile): ProjectFile {
  if (isModifiedFile(file)) {
    switch (file.type) {
      case 'CODE_FILE':
        return {
          ...file,
          fileContents: file.lastSavedContents,
          lastSavedContents: null,
        } as CodeFile
      case 'UI_JS_FILE':
        return {
          ...file,
          fileContents: file.lastSavedContents,
          lastSavedContents: null,
        } as UIJSFile
      default:
        throw new Error(`Only code and uijs files can be modified`)
    }
  }
  return file
}

export function saveFile(file: ProjectFile): ProjectFile {
  if (isModifiedFile(file)) {
    switch (file.type) {
      case 'CODE_FILE':
        return saveCodeFileContents(file, file.fileContents, true)
      case 'UI_JS_FILE':
        return saveUIJSFileContents(file, file.fileContents, true, file.revisionsState)
      default:
        throw new Error(`Only code and uijs files can be modified`)
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
