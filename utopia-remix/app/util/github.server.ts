import { Octokit } from '@octokit/rest'
import type { RequestInterface } from '@octokit/types'
import type {
  ProjectContentTreeRoot,
  ProjectContentDirectory,
  ExistingAsset,
  Content,
  ApiSuccess,
} from '../types'
import {
  toApiSuccess,
  isResponseWithMessageData,
  toApiFailure,
  projectContentDirectory,
  assetFile,
  imageFile,
  textFile,
  projectContentFile,
} from '../types'
import { Status } from './statusCodes'
import { json } from '@remix-run/node'
import * as fs from 'fs'
import urlJoin from 'url-join'
import * as unzipper from 'unzipper'
import { createHash } from 'crypto'
import path from 'path'
import { ServerEnvironment } from '../env.server'
import * as os from 'os'
import type { FileType } from './files'
import { fileTypeFromFileName } from './files'
import { assertNever } from './assertNever'
import AWS from 'aws-sdk'

export interface OctokitClient {
  request: RequestInterface
}

export function newOctokitClient(auth: string): OctokitClient {
  return new Octokit({ auth: auth })
}

export async function wrapGithubAPIRequest(
  client: OctokitClient,
  fn: (client: OctokitClient) => Promise<unknown>,
) {
  try {
    const result = await fn(client)
    return toApiSuccess(result)
  } catch (err) {
    return isResponseWithMessageData(err)
      ? json(toApiFailure(err.response.data.message), {
          status: err.status,
          headers: { 'cache-control': 'no-cache' },
        })
      : json(toApiFailure(`${err}`), {
          status: Status.INTERNAL_ERROR,
          headers: { 'cache-control': 'no-cache' },
        })
  }
}

export type AssetToUpload = {
  path: string
  data: Buffer
}

type UnzipResult = {
  projectContents: ProjectContentTreeRoot
  assetsToUpload: AssetToUpload[]
}

export type UnzipEntry = unzipper.Entry & {
  vars: {
    uncompressedSize: number // for some reason this field is not available in the type defs, but it's there
  }
}

export type BranchResponse = {
  branch: {
    branchName: string
    originCommit: string
    content: ProjectContentTreeRoot
  }
}

export function getBranchProjectContents(params: {
  owner: string
  repo: string
  branch: string
  projectId: string
  existingAssets: ExistingAsset[]
}) {
  return async function (client: OctokitClient): Promise<ApiSuccess<BranchResponse>> {
    // 1. get the branch details
    const response = await client.request('GET /repos/{owner}/{repo}/branches/{branch}', {
      owner: params.owner,
      repo: params.repo,
      branch: params.branch,
    })
    const commit = response.data.commit.sha

    // 2. get the zipball
    const tarball = await client.request('GET /repos/{owner}/{repo}/zipball/{ref}', {
      owner: params.owner,
      repo: params.repo,
      ref: commit,
    })

    // 3. write the zipball to disk in a temporary folder
    const { archiveName, zipFilePath } = writeZipballToTempFile({
      commit: commit,
      owner: params.owner,
      repo: params.repo,
      data: tarball.data as ArrayBuffer,
    })

    // 4. unzip the archive and process its entries
    const { assetsToUpload, projectContents } = await unzipGithubArchive({
      archiveName: archiveName,
      zipFilePath: zipFilePath,
      existingAssets: params.existingAssets,
    })

    // 6. if there are any assets to upload, upload them
    await uploadAssets({
      assets: assetsToUpload,
      projectId: params.projectId,
    })

    return toApiSuccess({
      branch: {
        branchName: params.branch,
        originCommit: commit,
        content: projectContents,
      },
    })
  }
}

function writeZipballToTempFile(params: {
  commit: string
  owner: string
  repo: string
  data: ArrayBuffer
}): { archiveName: string; zipFilePath: string } {
  const tempDir = os.tmpdir()

  const shortSha = params.commit.slice(0, 7)
  const archiveName = `${params.owner}-${params.repo}-${shortSha}`

  const zipFilePath = urlJoin(tempDir, archiveName + '.zip')
  fs.writeFileSync(zipFilePath, Buffer.from(params.data as ArrayBuffer))

  return {
    archiveName: archiveName,
    zipFilePath: zipFilePath,
  }
}

async function uploadAssets(params: { assets: AssetToUpload[]; projectId: string }): Promise<void> {
  // Locally, store the assets on disk.
  // On production, upload them to S3.
  const uploads = params.assets.map(async (asset) => {
    switch (ServerEnvironment.environment) {
      case 'local':
      case 'test':
        saveFileToDisk(params.projectId, asset)
        break
      case 'prod':
      case 'stage':
        saveFileToS3(params.projectId, asset)
        break
      default:
        assertNever(ServerEnvironment.environment)
    }
  })
  await Promise.all(uploads)
}

export function unzipGithubArchive(params: {
  archiveName: string
  zipFilePath: string
  existingAssets: ExistingAsset[]
}): Promise<UnzipResult> {
  return (
    new Promise<UnzipResult>((resolve) => {
      let assetsToUpload: AssetToUpload[] = []
      let root: ProjectContentDirectory = projectContentDirectory('')
      fs.createReadStream(params.zipFilePath)
        // NOTE: this is parsed in-memory (instead of unzipper.Extract), but if memory becomes a problem, we can easily convert this to be disk-backed
        .pipe(unzipper.Parse())
        // for each entry, process it and update the project contents, as well as adding assets to upload later
        .on('entry', processEntry(params.archiveName, root, params.existingAssets, assetsToUpload))
        .on('finish', () => {
          resolve({ projectContents: root.children, assetsToUpload: assetsToUpload })
        })
    })
      // 5. dispose of the zip file
      .finally(() => {
        fs.rmSync(params.zipFilePath)
      })
  )
}

export function processEntry(
  archiveName: string,
  root: ProjectContentDirectory,
  existingAssets: ExistingAsset[],
  assetsToUpload: AssetToUpload[],
) {
  return async function (entry: UnzipEntry): Promise<void> {
    // The entry path starts with the archive name, so get rid of it since we won't need it.
    const filePath = entry.path.replace(`${archiveName}/`, '')

    if (filePath.length > 0) {
      // Go through the folders that lead to this path and create their
      // representation in the tree, if missing.
      const target = populateDirectories(root, filePath)

      // Get the file contents and store them in the tree.
      await populateEntryContents(filePath, entry, target, existingAssets, assetsToUpload)
    }

    entry.autodrain()
  }
}

export function populateDirectories(root: ProjectContentDirectory, relativeFilePath: string) {
  const dirname = path.dirname(relativeFilePath)
  const folders = dirname.split('/').filter((folder) => folder !== '.')

  let target: ProjectContentDirectory = root
  for (let i = 0; i < folders.length; i++) {
    const folder = folders[i]
    if (target.children[folder] == null) {
      const folderPath = '/' + folders.slice(0, i + 1).join('/')
      target.children[folder] = projectContentDirectory(folderPath)
    }
    target = target.children[folder] as ProjectContentDirectory
  }

  return target
}

export async function populateEntryContents(
  filePath: string,
  entry: UnzipEntry,
  target: ProjectContentDirectory,
  existingAssets: ExistingAsset[],
  assetsToUpload: AssetToUpload[],
) {
  const base = path.basename(filePath)
  const filePathWithLeadingSlash = '/' + filePath.replace(/\/+$/, '')

  switch (entry.type) {
    case 'File':
      const buffer = await entry.buffer()
      const gitBlobSha = getGitBlobSha(entry.vars.uncompressedSize, buffer)
      const fileType = fileTypeFromFileName(filePath)

      // if the file needs to be uploaded later, add it to the list
      if (shouldUploadAsset(existingAssets, fileType, gitBlobSha, filePath)) {
        assetsToUpload.push({ path: filePath, data: buffer })
      }

      target.children[base] = projectContentFile(
        filePathWithLeadingSlash,
        fileContentFromEntry(buffer, gitBlobSha, fileType),
      )
      break
    case 'Directory':
      target.children[base] = projectContentDirectory(filePathWithLeadingSlash)
      break
    default:
      console.error(`unexpected entry type "${entry.type}"`)
  }
}

export function shouldUploadAsset(
  existingAssets: ExistingAsset[],
  fileType: FileType | null,
  gitBlobSha: string,
  filePath: string,
): boolean {
  const isUploadableType = fileType === 'ASSET_FILE' || fileType === 'IMAGE_FILE'
  if (!isUploadableType) {
    return false
  }

  const existingAsset = existingAssets.find((a) => a.path === filePath)
  return (
    // the file is new
    existingAsset == null ||
    // the type has changed
    existingAsset.type !== fileType ||
    // the checksum has changed
    existingAsset.gitBlobSha !== gitBlobSha
  )
}

function fileContentFromEntry(
  buffer: Buffer,
  gitBlobSha: string,
  fileType: FileType | null,
): Content {
  switch (fileType) {
    case 'ASSET_FILE':
      return assetFile({ gitBlobSha: gitBlobSha })
    case 'IMAGE_FILE':
      return imageFile({
        hash: 0, // TODO: I'm not really sure what this has is supposed to be for, how it's calculated, or if it's useful at all
        gitBlobSha: gitBlobSha,
      })
    case 'TEXT_FILE':
    case null:
      return textFile(buffer.toString())
  }
}

function getGitBlobSha(size: number, data: Buffer): string {
  const header = `blob ${size}\0`
  const blob = Buffer.concat([Buffer.from(header), data])
  const sha1 = createHash('sha1')
  sha1.update(blob)
  return sha1.digest('hex')
}

function saveFileToDisk(projectId: string, file: AssetToUpload) {
  const dir = path.dirname(file.path)
  const base = path.basename(file.path)
  const diskPath = urlJoin(ServerEnvironment.LOCAL_ASSETS_FOLDER, `/projects/${projectId}`, dir)
  fs.mkdirSync(diskPath, { recursive: true })
  const filePath = urlJoin(diskPath, base)
  fs.writeFileSync(filePath, file.data)
}

function projectFileS3Key(projectId: string, filePath: string): string {
  return `projects/${projectId}/${filePath}`
}

async function saveFileToS3(
  projectId: string,
  file: AssetToUpload,
): Promise<AWS.S3.ManagedUpload.SendData> {
  const s3 = new AWS.S3({
    accessKeyId: ServerEnvironment.AWS_ACCESS_KEY_ID,
    secretAccessKey: ServerEnvironment.AWS_SECRET_ACCESS_KEY,
    region: ServerEnvironment.AWS_REGION,
  })

  const params = {
    Bucket: ServerEnvironment.AWS_S3_BUCKET,
    Key: projectFileS3Key(projectId, file.path),
    Body: file.data,
  }

  return s3.upload(params).promise()
}
