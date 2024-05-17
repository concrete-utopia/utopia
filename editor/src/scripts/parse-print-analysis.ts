/* eslint no-console: "off" */
import * as Path from 'path'
import * as FS from 'fs'
import { applyPrettier } from 'utopia-vscode-common'
import {
  parseCode,
  printCode,
  printCodeOptions,
} from '../core/workers/parser-printer/parser-printer'
import { foldParsedTextFile } from '../core/shared/project-file-types'
import * as Diff from 'diff'
import type { GitRepoWithRevision } from './github-projects'
import {
  downloadAndExtractRepo,
  githubProjects,
  githubProjectsFileFilters,
} from './github-projects'
import minimatch from 'minimatch'

const javascriptFileEndings = ['.js', '.ts', '.jsx', '.tsx']

async function processFile(
  repo: GitRepoWithRevision,
  rootProjectPath: string,
  javascriptFilePath: string,
): Promise<string> {
  const fileContents = FS.readFileSync(javascriptFilePath, 'utf8')
  const initialPrettifiedContents = applyPrettier(fileContents, false).formatted
  const parsedContents = parseCode(
    javascriptFilePath,
    initialPrettifiedContents,
    null,
    'do-not-apply-steganography',
  )
  const printedContents = foldParsedTextFile(
    (_) => initialPrettifiedContents,
    (success) => {
      return printCode(
        javascriptFilePath,
        printCodeOptions(false, true, false, true),
        success.imports,
        success.topLevelElements,
        success.jsxFactoryFunction,
        success.exportsDetail,
      )
    },
    (_) => initialPrettifiedContents,
    parsedContents,
  )
  const pathForBefore = Path.join(
    repo.name,
    'before',
    javascriptFilePath.slice(rootProjectPath.length),
  )
  const pathForAfter = Path.join(
    repo.name,
    'after',
    javascriptFilePath.slice(rootProjectPath.length),
  )
  return Diff.createTwoFilesPatch(
    pathForBefore,
    pathForAfter,
    initialPrettifiedContents,
    printedContents,
  )
}

function allowedByGlobFilters(filePath: string): boolean {
  return githubProjectsFileFilters.every((filter) => {
    return !minimatch(filePath, filter)
  })
}

async function processProjectCode(
  repo: GitRepoWithRevision,
  rootProjectPath: string,
  targetDir: string,
): Promise<Array<string>> {
  let result: Array<string> = []
  // Even though we're opening it synchronously, the iterator is
  // asynchronous, because that makes sense.
  for await (const dirEntry of FS.opendirSync(targetDir)) {
    const fullPath = Path.join(targetDir, dirEntry.name)
    const fullPathWithoutRoot = fullPath.slice(rootProjectPath.length)
    // Some projects have built files in them which are potentially gigantic.
    // This allows us to filter them out of the results.
    if (
      !repo.pathsToIgnore.some((pathToIgnore) => pathToIgnore === fullPathWithoutRoot) &&
      allowedByGlobFilters(fullPathWithoutRoot)
    ) {
      // Walk down into the directory if it is one.
      if (dirEntry.isDirectory()) {
        const dirResult = await processProjectCode(repo, rootProjectPath, fullPath)
        result.push(...dirResult)
      }
      // Handle
      if (dirEntry.isFile() && javascriptFileEndings.some((ending) => fullPath.endsWith(ending))) {
        const fileResult = await processFile(repo, rootProjectPath, fullPath)
        if (fileResult.split('\n').length > 4) {
          // Skip empty patches
          result.push(fileResult)
        }
      }
    }
  }

  return result
}

async function processProject(gitRepo: GitRepoWithRevision): Promise<Array<string>> {
  // Create the folder that caches the current copy of the projects.
  const projectLocalDir = await downloadAndExtractRepo(gitRepo)

  return processProjectCode(gitRepo, projectLocalDir, projectLocalDir)
}

async function processProjects(): Promise<Array<string>> {
  const allDiffs = await Promise.all(githubProjects.map(processProject))
  return allDiffs.flat()
}

async function printAllDiffs(): Promise<void> {
  for (const diff of await processProjects()) {
    console.log(diff)
  }
}

void printAllDiffs()
