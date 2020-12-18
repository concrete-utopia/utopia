/* eslint no-console: "off" */
import * as Path from 'path'
import * as FS from 'fs'
import Got from 'got'
import { extract } from 'tar'
import { applyPrettier } from '../core/workers/parser-printer/prettier-utils'
import {
  parseCode,
  printCode,
  printCodeOptions,
} from '../core/workers/parser-printer/parser-printer'
import { foldParsedTextFile } from '../core/shared/project-file-types'
import * as Diff from 'diff'

const localDir = Path.resolve('./.parse-print-analysis/')

interface GitRepoWithRevision {
  name: string
  username: string
  repositoryName: string
  revision: string
  pathsToIgnore: Array<string>
}

const githubProjects: Array<GitRepoWithRevision> = [
  {
    name: 'react-three-flex-examples',
    username: 'utopia-test',
    repositoryName: 'react-three-flex-examples',
    revision: 'b4b60193de78a0f50e51028300c29bbdb67b34ab',
    pathsToIgnore: ['/public/draco-gltf', '/src/utils/three.js'],
  },
  {
    name: 'react-tic-tac-toe-with-hooks',
    username: 'utopia-test',
    repositoryName: 'react-tic-tac-toe-with-hooks',
    revision: 'd3eeb96536d9e7a78435b967a41d656164535528',
    pathsToIgnore: [],
  },
]

const javascriptFileEndings = ['.js', '.ts', '.jsx', '.tsx']

async function processFile(
  repo: GitRepoWithRevision,
  rootProjectPath: string,
  javascriptFilePath: string,
): Promise<string> {
  const fileContents = FS.readFileSync(javascriptFilePath, 'utf8')
  const initialPrettifiedContents = applyPrettier(fileContents, false).formatted
  const parsedContents = parseCode(javascriptFilePath, initialPrettifiedContents)
  const printedContents = foldParsedTextFile(
    (_) => initialPrettifiedContents,
    (success) => {
      return printCode(
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
    if (!repo.pathsToIgnore.some((pathToIgnore) => pathToIgnore === fullPathWithoutRoot)) {
      // Walk down into the directory if it is one.
      if (dirEntry.isDirectory()) {
        const dirResult = await processProjectCode(repo, rootProjectPath, fullPath)
        result.push(...dirResult)
      }
      // Handle
      if (dirEntry.isFile() && javascriptFileEndings.some((ending) => fullPath.endsWith(ending))) {
        const fileResult = await processFile(repo, rootProjectPath, fullPath)
        result.push(fileResult)
      }
    }
  }

  return result
}

async function processProject(gitRepo: GitRepoWithRevision): Promise<Array<string>> {
  // Create the folder that caches the current copy of the projects.
  if (!FS.existsSync(localDir)) {
    FS.mkdirSync(localDir, { recursive: true })
  }
  // Folder we intend on putting the contents into.
  const projectLocalDir = Path.join(localDir, gitRepo.revision)
  // Check if we already have a copy.
  if (!FS.existsSync(projectLocalDir)) {
    // If the local dir does not exist, download the tarball.
    const urlToDownload = `https://github.com/${gitRepo.username}/${gitRepo.repositoryName}/tarball/${gitRepo.revision}`
    const tarballLocalFile = Path.join(localDir, `${gitRepo.revision}.tgz`)
    const writeStream = FS.createWriteStream(tarballLocalFile)
    const writeStreamPromise = new Promise((resolve, reject) => {
      Got.stream(urlToDownload)
        .pipe(writeStream)
        .on('finish', () => resolve())
        .on('error', (error) => reject(error))
    })
    await writeStreamPromise
    // Expand the tarball.
    const targetDir = Path.join(localDir, gitRepo.revision)
    FS.mkdirSync(targetDir)
    // Extract the tarball into the folder for the revision,
    // stripping off the path prefix which contains the repo name
    // that we don't want.
    await extract({ file: tarballLocalFile, cwd: targetDir, strip: 1 })
  }

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

printAllDiffs()
