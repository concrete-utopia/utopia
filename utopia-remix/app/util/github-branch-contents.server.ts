import type { ExistingAsset, ApiSuccess } from '../types'
import {
  assetFile,
  imageFile,
  projectContentDirectory,
  projectContentFile,
  textFile,
  toApiSuccess,
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
import type { OctokitClient } from './github'
import * as uuid from 'uuid'
import { newS3Client, saveFileToDisk, saveFileToS3 } from './files.server'
import type {
  AssetFile,
  ImageFile,
  ProjectContentDirectory,
  ProjectContentTreeRoot,
  TextFile,
} from 'utopia-shared/src/types'
import type { Readable } from 'stream'

export type AssetToUpload = {
  path: string
  stream: () => Readable
  size: number
}

type UnzipResult = {
  projectContents: ProjectContentTreeRoot
  assetsToUpload: AssetToUpload[]
}

export type BranchResponse = {
  branch: {
    branchName: string
    originCommit: string
    content: ProjectContentTreeRoot
  }
  noChanges: boolean
}

export function getBranchProjectContents(params: {
  projectId: string
  owner: string
  repo: string
  branch: string
  uploadAssets: boolean
  existingAssets: ExistingAsset[]
  specificCommitSha: string | null
  previousCommitSha: string | null
}) {
  return async function (client: OctokitClient): Promise<ApiSuccess<BranchResponse>> {
    // 1. get the branch details
    async function getCommit() {
      if (params.specificCommitSha != null) {
        return params.specificCommitSha
      }
      const response = await client.request('GET /repos/{owner}/{repo}/branches/{branch}', {
        owner: params.owner,
        repo: params.repo,
        branch: params.branch,
      })
      return response.data.commit.sha
    }
    const commit = await getCommit()

    if (params.previousCommitSha != null && commit === params.previousCommitSha) {
      return toApiSuccess({
        branch: {
          branchName: params.branch,
          originCommit: params.previousCommitSha,
          content: {},
        },
        noChanges: true,
      })
    }

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

    try {
      // 4. unzip the archive and process its entries
      const existingAssets_MUTABLE = [...params.existingAssets]
      const { assetsToUpload, projectContents } = await unzipGithubArchive({
        archiveName: archiveName,
        zipFilePath: zipFilePath,
        existingAssets_MUTABLE: existingAssets_MUTABLE,
      })

      // 5. if there are any assets to upload, upload them
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
        noChanges: false,
      })
    } finally {
      // 6. dispose of the zip file
      fs.rmSync(zipFilePath)
    }
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
        await saveFileToS3(newS3Client(), params.projectId, asset)
        break
      default:
        assertNever(ServerEnvironment.environment)
    }
  })
  await Promise.all(uploads)
}

export async function unzipGithubArchive(params: {
  archiveName: string
  zipFilePath: string
  existingAssets_MUTABLE: ExistingAsset[]
}): Promise<UnzipResult> {
  let assetsToUpload_MUTABLE: AssetToUpload[] = []
  let root_MUTABLE: ProjectContentDirectory = projectContentDirectory('')
  const archive = await unzipper.Open.file(params.zipFilePath)
  for await (const file of archive.files) {
    await processArchiveFile(
      file,
      params.archiveName,
      root_MUTABLE,
      params.existingAssets_MUTABLE,
      assetsToUpload_MUTABLE,
    )
  }
  return {
    projectContents: root_MUTABLE.children,
    assetsToUpload: assetsToUpload_MUTABLE,
  }
}

export async function processArchiveFile(
  file: unzipper.File,
  archiveName: string,
  root_MUTABLE: ProjectContentDirectory,
  existingAssets_MUTABLE: ExistingAsset[],
  assetsToUpload_MUTABLE: AssetToUpload[],
): Promise<void> {
  // The archive path starts with the archive name, so get rid of it since we won't need it.
  const filePath = file.path.replace(`${archiveName}/`, '')
  if (filePath.length === 0) {
    return
  }

  // Go through the folders that lead to this path and create their
  // representation in the tree, if missing.
  const target = populateDirectories({ root_MUTABLE: root_MUTABLE, relativeFilePath: filePath })

  // Get the file contents and store them in the tree.
  await populateArchiveFileContents({
    file: file,
    filePath: filePath,
    target_MUTABLE: target,
    existingAssets_MUTABLE: existingAssets_MUTABLE,
    assetsToUpload_MUTABLE: assetsToUpload_MUTABLE,
  })
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

export async function populateArchiveFileContents(params: {
  file: unzipper.File
  filePath: string
  target_MUTABLE: ProjectContentDirectory
  existingAssets_MUTABLE: ExistingAsset[]
  assetsToUpload_MUTABLE: AssetToUpload[]
}) {
  const base = path.basename(params.filePath)
  const filePathWithLeadingSlash = '/' + params.filePath.replace(/\/+$/, '')

  switch (params.file.type) {
    case 'File':
      const gitBlobSha = await getGitBlobSha(params.file)
      const fileType = fileTypeFromFileName(params.filePath)

      // if the file needs to be uploaded later, add it to the list
      if (shouldUploadAsset(params.existingAssets_MUTABLE, fileType, gitBlobSha, params.filePath)) {
        params.assetsToUpload_MUTABLE.push({
          path: params.filePath,
          stream: params.file.stream,
          size: params.file.uncompressedSize,
        })
      }

      params.target_MUTABLE.children[base] = projectContentFile(
        filePathWithLeadingSlash,
        await projectFileContentFromArchiveFile(params.file, gitBlobSha, fileType),
      )
      break
    case 'Directory':
      params.target_MUTABLE.children[base] = projectContentDirectory(filePathWithLeadingSlash)
      break
    default:
      console.error(`unexpected file type "${params.file.type}"`)
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

async function projectFileContentFromArchiveFile(
  file: unzipper.File,
  gitBlobSha: string,
  fileType: FileType | null,
): Promise<ImageFile | TextFile | AssetFile> {
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
      const buffer = await file.buffer()
      return textFile(buffer.toString())
  }
}

async function getGitBlobSha(file: unzipper.File): Promise<string> {
  const sha1 = createHash('sha1')

  const header = `blob ${file.uncompressedSize}\0`
  sha1.update(header)

  const stream = file.stream()

  return new Promise((resolve, reject) => {
    stream.on('data', (data) => sha1.update(Buffer.from(data)))
    stream.on('error', reject)
    stream.on('end', () => resolve(sha1.digest('hex')))
  })
}
