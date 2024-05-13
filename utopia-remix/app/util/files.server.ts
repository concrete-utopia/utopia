import AWS from 'aws-sdk'
import * as path from 'path'
import * as fs from 'fs'
import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'
import type { AssetToUpload } from './github-branch-contents.server'

export async function saveFileToDisk(projectId: string, file: AssetToUpload) {
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

export function projectFileS3Key(projectId: string, filePath: string): string {
  return urlJoin('projects', projectId, filePath)
}

export function newS3Client(): AWS.S3 {
  let config: AWS.S3.Types.ClientConfiguration = {
    accessKeyId: ServerEnvironment.AWS_ACCESS_KEY_ID,
    secretAccessKey: ServerEnvironment.AWS_SECRET_ACCESS_KEY,
    region: ServerEnvironment.AWS_REGION,
  }
  if (ServerEnvironment.environment === 'test') {
    config.endpoint = new AWS.Endpoint('http://localhost:9000')
    config.s3ForcePathStyle = true
  }
  return new AWS.S3(config)
}

export async function saveFileToS3(
  client: AWS.S3,
  projectId: string,
  file: AssetToUpload,
): Promise<AWS.S3.ManagedUpload.SendData> {
  return client
    .upload({
      Bucket: ServerEnvironment.AWS_S3_BUCKET,
      Key: projectFileS3Key(projectId, file.path),
      Body: file.data,
    })
    .promise()
}
