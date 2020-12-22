/* eslint no-console: "off" */
import * as Path from 'path'
import * as FS from 'fs'
import Got from 'got'
import { extract } from 'tar'

export const localDir = Path.resolve('./.github-test-projects/')

export interface GitRepoWithRevision {
  name: string
  username: string
  repositoryName: string
  revision: string
  pathsToIgnore: Array<string>
}

export const githubProjects: Array<GitRepoWithRevision> = [
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

export async function downloadAndExtractRepo(gitRepo: GitRepoWithRevision): Promise<string> {
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
  return projectLocalDir
}
