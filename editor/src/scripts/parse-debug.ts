/* eslint no-console: "off" */

import * as Path from 'path'
import * as FS from 'fs'
import { downloadAndExtractRepo, githubProjects } from './github-projects'
import { applyPrettier } from 'utopia-vscode-common'
import { parseCode } from '../core/workers/parser-printer/parser-printer'
import { foldParsedTextFile } from '../core/shared/project-file-types'
import { elementsStructure } from '../core/workers/parser-printer/parser-printer.test-utils'
import { emptySet } from '../core/shared/set-utils'

async function printOutParseResult(
  command: string,
  gitRepoName: string,
  javascriptFilePath: string,
): Promise<void> {
  const gitRepo = githubProjects.find((project) => project.name === gitRepoName)
  if (gitRepo == null) {
    console.error(`Could not find repo with name ${gitRepoName}`)
  } else {
    const repoDir = await downloadAndExtractRepo(gitRepo)
    const fullFilePath = Path.join(repoDir, javascriptFilePath)
    if (FS.existsSync(fullFilePath)) {
      const fileContents = FS.readFileSync(fullFilePath, 'utf8')
      const initialPrettifiedContents = applyPrettier(fileContents, false).formatted
      const parsedContents = parseCode(
        javascriptFilePath,
        [],
        initialPrettifiedContents,
        null,
        emptySet(),
        'do-not-apply-steganography',
      )
      switch (command) {
        case 'print':
          console.log(JSON.stringify(parsedContents, null, 2))
          break
        case 'structure':
          const result = foldParsedTextFile(
            (failure) => failure.errorMessage,
            (success) => elementsStructure(success.topLevelElements),
            () => 'UNPARSED',
            parsedContents,
          )
          console.log(result)
          break
        default:
          console.error(`Unhandled command ${command}.`)
      }
    } else {
      console.error(`Could not find file ${javascriptFilePath} in ${gitRepoName}`)
    }
  }
}

let processArguments = [...process.argv]
// Strip away the weird set of process arguments we get because of ts-node.
if (processArguments.length === 5 && processArguments[0].endsWith('ts-node')) {
  processArguments = processArguments.slice(2)
}

if (processArguments.length === 3) {
  void printOutParseResult(processArguments[0], processArguments[1], processArguments[2])
} else {
  console.error('Invalid number of parameters. Must be command, repo name and then filename.')
}
