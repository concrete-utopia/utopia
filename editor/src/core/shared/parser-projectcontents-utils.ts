import type {
  ParseOrPrint,
  ParseOrPrintResult,
  UtopiaTsWorkers,
} from '../../core/workers/common/worker-types'
import {
  createParseFile,
  createPrintAndReparseFile,
  getParseResult,
} from '../../core/workers/common/worker-types'
import * as EditorActions from '../../components/editor/actions/action-creators'
import type { WorkerUpdate } from '../../components/editor/action-types'
import type { ProjectContentTreeRoot } from '../.././components/assets'
import {
  addFileToProjectContents,
  getProjectFileByFilePath,
  walkContentsTree,
} from '../../components/assets'
import {
  getHighlightBoundsFromParseResult,
  updateParsedTextFileHighlightBounds,
} from '../../core/model/project-file-utils'
import type { TextFile, ParsedTextFile } from '../../core/shared/project-file-types'
import {
  isTextFile,
  textFile,
  textFileContents,
  isParseSuccess,
  RevisionsState,
} from '../../core/shared/project-file-types'
import { PRODUCTION_ENV } from '../../common/env-vars'
import { emptySet } from '../../core/shared/set-utils'
import { fastForEach } from '../../core/shared/utils'
import { codeNeedsPrinting, codeNeedsParsing } from '../../core/workers/common/project-file-utils'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { isSteganographyEnabled } from './stegano-text'

export function parseResultToWorkerUpdates(fileResult: ParseOrPrintResult): WorkerUpdate {
  switch (fileResult.type) {
    case 'parsefileresult':
      return EditorActions.workerParsedUpdate(
        fileResult.filename,
        fileResult.parseResult,
        fileResult.versionNumber,
      )
    case 'printandreparseresult':
      return EditorActions.workerCodeAndParsedUpdate(
        fileResult.filename,
        fileResult.printResult,
        fileResult.parseResult,
        fileResult.versionNumber,
      )
    default:
      const _exhaustiveCheck: never = fileResult
      throw new Error(`Unhandled file result ${JSON.stringify(fileResult)}`)
  }
}

export interface ProcessWorkerUpdatesResult {
  anyParsedUpdates: boolean
  projectContents: ProjectContentTreeRoot
}

export function processWorkerUpdates(
  projectContents: ProjectContentTreeRoot,
  workerUpdates: Array<WorkerUpdate>,
): ProcessWorkerUpdatesResult {
  let anyParsedUpdates: boolean = false
  let workingProjectContents: ProjectContentTreeRoot = projectContents
  for (const fileUpdate of workerUpdates) {
    const existing = getProjectFileByFilePath(projectContents, fileUpdate.filePath)
    if (existing != null && isTextFile(existing)) {
      anyParsedUpdates = true
      let updatedFile: TextFile
      let updatedContents: ParsedTextFile
      let code: string
      const updateIsStale = fileUpdate.versionNumber < existing.versionNumber
      switch (fileUpdate.type) {
        case 'WORKER_PARSED_UPDATE': {
          code = existing.fileContents.code
          const highlightBounds = getHighlightBoundsFromParseResult(fileUpdate.parsed)
          updatedContents = updateParsedTextFileHighlightBounds(fileUpdate.parsed, highlightBounds)
          break
        }
        case 'WORKER_CODE_AND_PARSED_UPDATE':
          code = fileUpdate.code
          const highlightBounds = getHighlightBoundsFromParseResult(fileUpdate.parsed)
          // Because this will print and reparse, we need to be careful of changes to the parsed
          // model that have happened since we requested this update
          updatedContents = updateIsStale
            ? existing.fileContents.parsed
            : updateParsedTextFileHighlightBounds(fileUpdate.parsed, highlightBounds)
          break
        default:
          const _exhaustiveCheck: never = fileUpdate
          throw new Error(`Invalid file update: ${fileUpdate}`)
      }

      if (updateIsStale) {
        // if the received file is older than the existing, we still allow it to update the other side,
        // but we don't bump the revision state.
        updatedFile = textFile(
          textFileContents(code, updatedContents, existing.fileContents.revisionsState),
          existing.lastSavedContents,
          isParseSuccess(updatedContents) ? updatedContents : existing.lastParseSuccess,
          existing.versionNumber,
        )
      } else {
        updatedFile = textFile(
          textFileContents(code, updatedContents, RevisionsState.BothMatch),
          existing.lastSavedContents,
          isParseSuccess(updatedContents) ? updatedContents : existing.lastParseSuccess,
          existing.versionNumber,
        )
      }

      workingProjectContents = addFileToProjectContents(
        workingProjectContents,
        fileUpdate.filePath,
        updatedFile,
      )
    } else {
      // The worker shouldn't be recreating deleted files or reformated files
      console.error(`Worker thread is trying to update an invalid file ${fileUpdate.filePath}`)
    }
  }
  return {
    anyParsedUpdates: anyParsedUpdates,
    projectContents: workingProjectContents,
  }
}

export interface GetFilesToUpdateResult {
  filesToUpdate: Array<ParseOrPrint>
  forciblyParsedFiles: Array<string>
  existingUIDs: Set<string>
}

export function getFilesToUpdate(
  projectContents: ProjectContentTreeRoot,
  forceParseFiles: Array<string>,
): GetFilesToUpdateResult {
  // Walk the project contents to see if anything needs to be sent across.
  let filesToUpdate: Array<ParseOrPrint> = []
  let forciblyParsedFiles: Array<string> = []
  let existingUIDs: Set<string> = emptySet()
  walkContentsTree(projectContents, (fullPath, file) => {
    if (isTextFile(file)) {
      if (
        codeNeedsPrinting(file.fileContents.revisionsState) &&
        isParseSuccess(file.fileContents.parsed)
      ) {
        filesToUpdate.push(
          createPrintAndReparseFile(fullPath, file.fileContents.parsed, true, file.versionNumber),
        )
      } else if (codeNeedsParsing(file.fileContents.revisionsState)) {
        const lastParseSuccess = isParseSuccess(file.fileContents.parsed)
          ? file.fileContents.parsed
          : file.lastParseSuccess
        filesToUpdate.push(
          createParseFile(fullPath, file.fileContents.code, lastParseSuccess, file.versionNumber),
        )
      } else if (forceParseFiles.includes(fullPath)) {
        forciblyParsedFiles.push(fullPath)
        const lastParseSuccess = isParseSuccess(file.fileContents.parsed)
          ? file.fileContents.parsed
          : file.lastParseSuccess
        filesToUpdate.push(
          createParseFile(fullPath, file.fileContents.code, lastParseSuccess, file.versionNumber),
        )
      } else if (isParseSuccess(file.fileContents.parsed)) {
        const uidsFromFile = Object.keys(file.fileContents.parsed.fullHighlightBounds)
        fastForEach(uidsFromFile, (uid) => existingUIDs.add(uid))
      }
    }
  })

  return {
    filesToUpdate: filesToUpdate,
    forciblyParsedFiles: forciblyParsedFiles,
    existingUIDs: existingUIDs,
  }
}

export async function updateProjectContentsWithParseResults(
  workers: UtopiaTsWorkers,
  projectContents: ProjectContentTreeRoot,
): Promise<ProjectContentTreeRoot> {
  // Determine what files need updating.
  const { filesToUpdate, existingUIDs } = getFilesToUpdate(projectContents, [])

  // Get the result of parsing or printing the files.
  const parseResult = await getParseResult(
    workers,
    filesToUpdate,
    existingUIDs,
    isSteganographyEnabled(),
  )

  // Convert those results into updates.
  const workerUpdates = parseResult.map(parseResultToWorkerUpdates)

  // Update the project contents by processing the worker updates in order.
  const workerUpdatesResult = processWorkerUpdates(projectContents, workerUpdates)

  // Return the result.
  return workerUpdatesResult.projectContents
}
