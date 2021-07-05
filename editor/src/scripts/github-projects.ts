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

export const githubProjectsFileFilters: Array<string> = ['**/__tests__']

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
  {
    name: 'ant-design',
    username: 'utopia-test',
    repositoryName: 'ant-design',
    revision: '739f87ed3dcf613214e7504cf712ac9f1c03d315',
    pathsToIgnore: ['tests'],
  },
  {
    name: 'react-select',
    username: 'JedWatson',
    repositoryName: 'react-select',
    revision: 'd1e660c6b261d7fd60a85a6eca2ee9e3e0348ea2',
    pathsToIgnore: [],
  },
  {
    name: 'react-data-grid',
    username: 'adazzle',
    repositoryName: 'react-data-grid',
    revision: 'd7e2ba8be54931d69b6e58667813c8f8448f00e9',
    pathsToIgnore: [],
  },
  {
    name: 'react-resizable',
    username: 'STRML',
    repositoryName: 'react-resizable',
    revision: '09fd865c0e1cc570caa8d67e44a2e56172d3d816',
    pathsToIgnore: [],
  },
  {
    name: 'react-dates',
    username: 'airbnb',
    repositoryName: 'react-dates',
    revision: 'b66dbe4397c7307946abecb10bf37914ebc746f9',
    pathsToIgnore: [],
  },
  {
    name: 'react-big-calendar',
    username: 'jquense',
    repositoryName: 'react-big-calendar',
    revision: '8ffe39dccce5ec7bcac618f9494c21ae557b3537',
    pathsToIgnore: [],
  },
  {
    name: 'react-responsive',
    username: 'contra',
    repositoryName: 'react-responsive',
    revision: 'b6ffef5c4a4b89c69299c2b04c9196dde59b3ef4',
    pathsToIgnore: ['/samples/sandbox/dist/sample.js'],
  },
  {
    name: 'context-provider-hooks-sample',
    username: 'maltenuhn',
    repositoryName: 'context-provider-hooks-sample',
    revision: 'a9155e332fd9902cc9c122392234dedba8c47d9a',
    pathsToIgnore: [],
  },
  {
    name: 'constate',
    username: 'diegohaz',
    repositoryName: 'constate',
    revision: '6858b1cb93f398359851af3a48a07ec47c8292a1',
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
    const writeStreamPromise = new Promise<void>((resolve, reject) => {
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
