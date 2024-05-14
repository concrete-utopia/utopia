import type { S3Client } from '@aws-sdk/client-s3'
import { PutObjectCommand, S3 } from '@aws-sdk/client-s3'
import * as path from 'path'
import * as fs from 'fs'
import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'
import type { AssetToUpload } from './github-branch-contents.server'
import * as stream from 'stream'

export async function saveFileToDisk(projectId: string, file: AssetToUpload) {
  // create destination directory
  const dir = path.dirname(file.path)
  const diskPath = urlJoin(ServerEnvironment.LOCAL_ASSETS_FOLDER, `/projects/${projectId}`, dir)
  fs.mkdirSync(diskPath, { recursive: true })

  // derive destination file base name
  const base = path.basename(file.path)
  const filePath = urlJoin(diskPath, base)

  // write the file stream into the file path
  return new Promise<void>((resolve, reject) =>
    stream.pipeline(file.stream(), fs.createWriteStream(filePath), (err) => {
      if (err != null) {
        reject(err)
      } else {
        resolve()
      }
    }),
  )
}

export function projectFileS3Key(projectId: string, filePath: string): string {
  return urlJoin('projects', projectId, filePath)
}

export function newS3Client(): S3Client {
  const isTestEnv = ServerEnvironment.environment === 'test'
  return new S3({
    credentials: {
      accessKeyId: ServerEnvironment.AWS_ACCESS_KEY_ID,
      secretAccessKey: ServerEnvironment.AWS_SECRET_ACCESS_KEY,
    },
    region: ServerEnvironment.AWS_REGION,
    endpoint: isTestEnv ? 'http://localhost:9000' : undefined,
    forcePathStyle: isTestEnv,
  })
}

export async function saveFileToS3(client: S3Client, projectId: string, asset: AssetToUpload) {
  return client.send(
    new PutObjectCommand({
      Bucket: ServerEnvironment.AWS_S3_BUCKET,
      Key: projectFileS3Key(projectId, asset.path),
      Body: asset.stream(),
      ContentLength: asset.size,
    }),
  )
}
