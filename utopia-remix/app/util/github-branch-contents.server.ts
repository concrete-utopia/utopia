import type {
  ProjectContentTreeRoot,
  ProjectContentDirectory,
  ExistingAsset,
  Content,
  ApiSuccess,
} from '../types'
import {
  toApiSuccess,
  projectContentDirectory,
  assetFile,
  imageFile,
  textFile,
  projectContentFile,
} from '../types'
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
import type { OctokitClient } from './github'
import * as uuid from 'uuid'

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
  projectId: string
  owner: string
  repo: string
  branch: string
  uploadAssets: boolean
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
    const { archiveName, zipFilePath } = await writeZipballToTempFile({
      commit: commit,
      owner: params.owner,
      repo: params.repo,
      data: tarball.data as ArrayBuffer,
    })

    // 4. unzip the archive and process its entries
    const existingAssets_MUTABLE = [...params.existingAssets]
    const { assetsToUpload, projectContents } = await unzipGithubArchive({
      archiveName: archiveName,
      zipFilePath: zipFilePath,
      existingAssets_MUTABLE: existingAssets_MUTABLE,
    })

    // 6. if there are any assets to upload, upload them
    if (params.uploadAssets) {
      await uploadAssets({
        assets: assetsToUpload,
        projectId: params.projectId,
      })
    }

    return toApiSuccess({
      branch: {
        branchName: params.branch,
        originCommit: commit,
        content: projectContents,
      },
    })
  }
}

async function writeZipballToTempFile(params: {
  commit: string
  owner: string
  repo: string
  data: ArrayBuffer
}): Promise<{ archiveName: string; zipFilePath: string }> {
  const tempDir = os.tmpdir()

  const shortSha = params.commit.slice(0, 7)
  const archiveName = `${params.owner}-${params.repo}-${shortSha}`

  const zipFileName = `${archiveName}-${uuid.v4()}.zip`
  const zipFilePath = urlJoin(tempDir, zipFileName)

  return new Promise((resolve, reject) => {
    fs.writeFile(zipFilePath, Buffer.from(params.data as ArrayBuffer), (err) => {
      if (err != null) {
        reject(err)
      } else {
        resolve({
          archiveName: archiveName,
          zipFilePath: zipFilePath,
        })
      }
    })
  })
}

async function uploadAssets(params: { assets: AssetToUpload[]; projectId: string }): Promise<void> {
  // Locally, store the assets on disk.
  // On production, upload them to S3.
  const uploads = params.assets.map(async (asset) => {
    switch (ServerEnvironment.environment) {
      case 'local':
      case 'test':
        await saveFileToDisk(params.projectId, asset)
        break
      case 'prod':
      case 'stage':
        await saveFileToS3(params.projectId, asset)
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
  existingAssets_MUTABLE: ExistingAsset[]
}): Promise<UnzipResult> {
  return (
    new Promise<UnzipResult>((resolve) => {
      let assetsToUpload_MUTABLE: AssetToUpload[] = []
      let root_MUTABLE: ProjectContentDirectory = projectContentDirectory('')
      fs.createReadStream(params.zipFilePath)
        // NOTE: this is parsed in-memory (instead of unzipper.Extract), but if memory becomes a problem, we can easily convert this to be disk-backed
        .pipe(unzipper.Parse())
        // for each entry, process it and update the project contents, as well as adding assets to upload later
        .on(
          'entry',
          processEntry(
            params.archiveName,
            root_MUTABLE,
            params.existingAssets_MUTABLE,
            assetsToUpload_MUTABLE,
          ),
        )
        .on('finish', () => {
          resolve({
            projectContents: root_MUTABLE.children,
            assetsToUpload: assetsToUpload_MUTABLE,
          })
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
  root_MUTABLE: ProjectContentDirectory,
  existingAssets_MUTABLE: ExistingAsset[],
  assetsToUpload_MUTABLE: AssetToUpload[],
) {
  return async function (entry: UnzipEntry): Promise<void> {
    // The entry path starts with the archive name, so get rid of it since we won't need it.
    const filePath = entry.path.replace(`${archiveName}/`, '')

    if (filePath.length > 0) {
      // Go through the folders that lead to this path and create their
      // representation in the tree, if missing.
      const target = populateDirectories({ root_MUTABLE: root_MUTABLE, relativeFilePath: filePath })

      // Get the file contents and store them in the tree.
      await populateEntryContents({
        filePath: filePath,
        entry: entry,
        target_MUTABLE: target,
        existingAssets_MUTABLE: existingAssets_MUTABLE,
        assetsToUpload_MUTABLE: assetsToUpload_MUTABLE,
      })
    }

    entry.autodrain()
  }
}

export function populateDirectories(params: {
  root_MUTABLE: ProjectContentDirectory
  relativeFilePath: string
}): ProjectContentDirectory {
  const dirname = path.dirname(params.relativeFilePath)
  const folders = dirname.split('/').filter((folder) => folder !== '.')

  let target: ProjectContentDirectory = params.root_MUTABLE
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

export async function populateEntryContents(params: {
  filePath: string
  entry: UnzipEntry
  target_MUTABLE: ProjectContentDirectory
  existingAssets_MUTABLE: ExistingAsset[]
  assetsToUpload_MUTABLE: AssetToUpload[]
}) {
  const base = path.basename(params.filePath)
  const filePathWithLeadingSlash = '/' + params.filePath.replace(/\/+$/, '')

  switch (params.entry.type) {
    case 'File':
      const buffer = await params.entry.buffer()
      const gitBlobSha = getGitBlobSha(params.entry.vars.uncompressedSize, buffer)
      const fileType = fileTypeFromFileName(params.filePath)

      // if the file needs to be uploaded later, add it to the list
      if (shouldUploadAsset(params.existingAssets_MUTABLE, fileType, gitBlobSha, params.filePath)) {
        params.assetsToUpload_MUTABLE.push({ path: params.filePath, data: buffer })
      }

      params.target_MUTABLE.children[base] = projectContentFile(
        filePathWithLeadingSlash,
        fileContentFromEntry(buffer, gitBlobSha, fileType),
      )
      break
    case 'Directory':
      params.target_MUTABLE.children[base] = projectContentDirectory(filePathWithLeadingSlash)
      break
    default:
      console.error(`unexpected entry type "${params.entry.type}"`)
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

async function saveFileToDisk(projectId: string, file: AssetToUpload) {
  const dir = path.dirname(file.path)
  const base = path.basename(file.path)
  const diskPath = urlJoin(ServerEnvironment.LOCAL_ASSETS_FOLDER, `/projects/${projectId}`, dir)
  fs.mkdirSync(diskPath, { recursive: true })
  const filePath = urlJoin(diskPath, base)
  return new Promise((resolve, reject) =>
    fs.writeFile(filePath, file.data, (err) => {
      if (err != null) {
        reject(err)
      } else {
        resolve(filePath)
      }
    }),
  )
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
