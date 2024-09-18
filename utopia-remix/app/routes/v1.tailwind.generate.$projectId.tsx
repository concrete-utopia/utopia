import type { ActionFunctionArgs } from '@remix-run/node'
import { json } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import * as child_process from 'child_process'
import fs from 'fs'
import path from 'path'
import util from 'util'
import type {
  AssetFile,
  ImageFile,
  ProjectContentTreeRoot,
  TextFile,
} from 'utopia-shared/src/types'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'
import { ensure, handle } from '../util/api.server'
import { assertNever } from '../util/assertNever'
import { Status } from '../util/statusCodes'
import { ServerEnvironment } from '../env.server'

/// UTILs

const exec = util.promisify(child_process.exec)

const redirectOutput = (
  promiseWithChild: child_process.PromiseWithChild<{
    stdout: string
    stderr: string
  }>,
) => {
  promiseWithChild.child.stdout?.on('data', (data) => {
    process.stdout.write(data)
  })
  promiseWithChild.child.stderr?.on('data', (data) => {
    process.stderr.write(data)
  })
  return promiseWithChild
}

async function walkContentsFiles(
  tree: ProjectContentTreeRoot,
  onElement: (fullPath: string, file: TextFile | ImageFile | AssetFile) => Promise<void>,
): Promise<void> {
  const treeKeys = Object.keys(tree)
  treeKeys.sort()
  treeKeys.forEach(async (treeKey) => {
    const treeElement = tree[treeKey]
    switch (treeElement.type) {
      case 'PROJECT_CONTENT_FILE':
        onElement(treeElement.fullPath, treeElement.content)
        break
      case 'PROJECT_CONTENT_DIRECTORY':
        await walkContentsFiles(treeElement.children, onElement)
        break
      default:
        assertNever(treeElement)
    }
  })
}

async function withTempFolder<T>(
  folderName: string,
  callback: (folderName: string) => Promise<T>,
): Promise<T> {
  const tempFolder = `/tmp/${folderName}`
  await fs.promises.mkdir(tempFolder, { recursive: true })
  try {
    return await callback(tempFolder)
  } finally {
    await fs.promises.rm(tempFolder, { recursive: true, force: true })
  }
}

/// HANDLER

async function fetchProjectContents(projectId: string): Promise<ProjectContentTreeRoot> {
  const response = await fetch(
    `${ServerEnvironment.BACKEND_URL}/v1/project/${projectId}/contents.json`,
  )
  const data = await response.json()
  return data.projectContents
}

async function writeProjectContentsToDisk(
  rootFolderName: string,
  projectContents: ProjectContentTreeRoot,
): Promise<void> {
  await walkContentsFiles(projectContents, async (fullPath, file) => {
    if (file.type !== 'TEXT_FILE') {
      return
    }
    const filePath = path.join(rootFolderName, fullPath)
    await fs.promises.mkdir(path.dirname(filePath), { recursive: true })
    await fs.promises.writeFile(filePath, file.fileContents.code)
  })
}

async function readGeneratedTailwindFileContents(outputFileName: string): Promise<string> {
  return await fs.promises.readFile(outputFileName, 'utf-8')
}

async function handleGenerateTailwindStyles(request: Request, params: Params<string>) {
  const projectId = params.projectId
  ensure(projectId != null, 'invalid project id', Status.BAD_REQUEST)

  const projectContentsTree = await fetchProjectContents(projectId)

  const contents = await withTempFolder(projectId, async (projectFolderPath) => {
    await writeProjectContentsToDisk(projectFolderPath, projectContentsTree)

    await redirectOutput(exec(`cd ${projectFolderPath} && yarn install --ignore-scripts`))

    const buildCSSPath = path.join(projectFolderPath, 'build.css')

    await redirectOutput(
      exec(`cd ${projectFolderPath} && npx tailwindcss -i ./app/styles/app.css -o ${buildCSSPath}`),
    )

    return await readGeneratedTailwindFileContents(buildCSSPath)
  })

  return json({ css: contents })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.projectId,
      }),
      handler: handleGenerateTailwindStyles,
    },
  })
}
