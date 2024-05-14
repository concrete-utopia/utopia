import urlJoin from 'url-join'
import type { AssetToUpload } from './github-branch-contents.server'
import {
  populateDirectories,
  populateArchiveFileContents,
  processArchiveFile,
  shouldUploadAsset,
  unzipGithubArchive,
} from './github-branch-contents.server'
import * as fs from 'fs'
import * as os from 'os'
import path from 'path'
import { projectContentDirectory, type ExistingAsset } from '../types'
import type { ProjectContentDirectory } from 'utopia-shared/src/types'
import type * as unzipper from 'unzipper'
import { readableStream } from '../test-util'
import { TestProjectUtopiaGithubMain } from '../test-assets/test-utopia-github-main'

describe('Github get branch contents', () => {
  describe('shouldUploadAsset', () => {
    const existingAssets: ExistingAsset[] = [
      { gitBlobSha: 'foo', path: 'foo.bin', type: 'ASSET_FILE' },
      { gitBlobSha: 'bar', path: 'bar.png', type: 'IMAGE_FILE' },
    ]
    it('returns false for non uploadable assets', async () => {
      const got = shouldUploadAsset(existingAssets, 'TEXT_FILE', 'somehthing', 'something.txt')
      expect(got).toBe(false)
    })
    it('returns true for new files', async () => {
      const got = shouldUploadAsset(existingAssets, 'ASSET_FILE', 'imnew', 'imnew.bin')
      expect(got).toBe(true)
    })
    it('returns true for files that changed their type', async () => {
      const got = shouldUploadAsset(existingAssets, 'ASSET_FILE', 'bar', 'bar.png')
      expect(got).toBe(true)
    })
    it('returns true if the checksum changed', async () => {
      const got = shouldUploadAsset(existingAssets, 'IMAGE_FILE', 'QUX', 'bar.png')
      expect(got).toBe(true)
    })
    it('returns false otherwise', async () => {
      const got = shouldUploadAsset(existingAssets, 'IMAGE_FILE', 'bar', 'bar.png')
      expect(got).toBe(false)
    })
  })

  describe('populateDirectories', () => {
    it('populates the directories for an entry', async () => {
      let root = projectContentDirectory('root')

      const target = populateDirectories({
        root_MUTABLE: root,
        relativeFilePath: 'some/file/here.png',
      })

      expect(root.children).toEqual({
        some: {
          children: {
            file: {
              children: {},
              directory: { type: 'DIRECTORY' },
              fullPath: '/some/file',
              type: 'PROJECT_CONTENT_DIRECTORY',
            },
          },
          directory: { type: 'DIRECTORY' },
          fullPath: '/some',
          type: 'PROJECT_CONTENT_DIRECTORY',
        },
      })
      expect(target).toBe((root.children['some'] as ProjectContentDirectory).children['file'])
    })
  })

  describe('populateArchiveFileContents', () => {
    it('populates the leaf for a directory entry', async () => {
      let root = projectContentDirectory('root')
      const target = populateDirectories({ root_MUTABLE: root, relativeFilePath: 'some/dir/here' })
      await populateArchiveFileContents({
        filePath: 'some/dir/here',
        file: { type: 'Directory' } as unzipper.File,
        target_MUTABLE: target,
        existingAssets_MUTABLE: [],
        assetsToUpload_MUTABLE: [],
      })
      expect(root.children).toEqual({
        some: {
          children: {
            dir: {
              children: {
                here: {
                  children: {},
                  directory: { type: 'DIRECTORY' },
                  fullPath: '/some/dir/here',
                  type: 'PROJECT_CONTENT_DIRECTORY',
                },
              },
              directory: { type: 'DIRECTORY' },
              fullPath: '/some/dir',
              type: 'PROJECT_CONTENT_DIRECTORY',
            },
          },
          directory: { type: 'DIRECTORY' },
          fullPath: '/some',
          type: 'PROJECT_CONTENT_DIRECTORY',
        },
      })
    })
    it('populates the leaf for a file entry', async () => {
      let root = projectContentDirectory('root')
      const target = populateDirectories({
        root_MUTABLE: root,
        relativeFilePath: 'some/file/here.json',
      })
      const data = `"hello there"`
      await populateArchiveFileContents({
        filePath: 'some/file/here.json',
        file: {
          type: 'File',
          uncompressedSize: data.length,
          stream: () => readableStream(data),
          buffer: async () => Buffer.from(data),
        } as unzipper.File,
        target_MUTABLE: target,
        existingAssets_MUTABLE: [],
        assetsToUpload_MUTABLE: [],
      })
      expect(root.children).toEqual({
        some: {
          children: {
            file: {
              children: {
                'here.json': {
                  content: {
                    fileContents: {
                      code: '"hello there"',
                      parsed: { type: 'UNPARSED' },
                      revisionsState: 'CODE_AHEAD',
                    },
                    lastParseSuccess: null,
                    lastSavedContents: null,
                    type: 'TEXT_FILE',
                    versionNumber: 0,
                  },
                  fullPath: '/some/file/here.json',
                  type: 'PROJECT_CONTENT_FILE',
                },
              },
              directory: { type: 'DIRECTORY' },
              fullPath: '/some/file',
              type: 'PROJECT_CONTENT_DIRECTORY',
            },
          },
          directory: { type: 'DIRECTORY' },
          fullPath: '/some',
          type: 'PROJECT_CONTENT_DIRECTORY',
        },
      })
    })
    it('adds a new file to upload if there is a need', async () => {
      let root = projectContentDirectory('root')
      const target = populateDirectories({
        root_MUTABLE: root,
        relativeFilePath: 'some/file/here.png',
      })

      const toUpload: AssetToUpload[] = []

      const data = `"hello there"`
      await populateArchiveFileContents({
        filePath: 'some/file/here.png',
        file: {
          type: 'File',
          uncompressedSize: data.length,
          stream: () => readableStream(data),
        } as unzipper.File,
        target_MUTABLE: target,
        existingAssets_MUTABLE: [],
        assetsToUpload_MUTABLE: toUpload,
      })

      expect(root.children).toEqual({
        some: {
          children: {
            file: {
              children: {
                'here.png': {
                  content: {
                    gitBlobSha: 'eb08f32ed19b72662acd4c30e361e38ca7d54daf', // <- this!
                    hash: 0,
                    type: 'IMAGE_FILE', // <- this!
                  },
                  fullPath: '/some/file/here.png',
                  type: 'PROJECT_CONTENT_FILE',
                },
              },
              directory: { type: 'DIRECTORY' },
              fullPath: '/some/file',
              type: 'PROJECT_CONTENT_DIRECTORY',
            },
          },
          directory: { type: 'DIRECTORY' },
          fullPath: '/some',
          type: 'PROJECT_CONTENT_DIRECTORY',
        },
      })
      expect(toUpload.length).toEqual(1)
      expect(toUpload[0].path).toEqual('some/file/here.png')
    })
  })

  describe('processArchiveFile', () => {
    it('processes the file', async () => {
      let root = projectContentDirectory('root')

      const data = `"hello there"`
      await processArchiveFile(
        {
          path: 'the-archive/some/file/here.json',
          type: 'File',
          uncompressedSize: data.length,
          stream: () => readableStream(data),
          buffer: async () => Buffer.from(data),
        } as unzipper.File,
        'the-archive',
        root,
        [],
        [],
      )

      expect(root.children).toEqual({
        some: {
          children: {
            file: {
              children: {
                'here.json': {
                  content: {
                    fileContents: {
                      code: '"hello there"',
                      parsed: { type: 'UNPARSED' },
                      revisionsState: 'CODE_AHEAD',
                    },
                    lastParseSuccess: null,
                    lastSavedContents: null,
                    type: 'TEXT_FILE',
                    versionNumber: 0,
                  },
                  fullPath: '/some/file/here.json',
                  type: 'PROJECT_CONTENT_FILE',
                },
              },
              directory: { type: 'DIRECTORY' },
              fullPath: '/some/file',
              type: 'PROJECT_CONTENT_DIRECTORY',
            },
          },
          directory: { type: 'DIRECTORY' },
          fullPath: '/some',
          type: 'PROJECT_CONTENT_DIRECTORY',
        },
      })
    })
  })

  describe('unzipGithubArchive', () => {
    it('unzips the archive', async () => {
      const dirname = path.dirname(__filename)

      const archiveName = `test-utopia-github-main`

      const testFilePath = urlJoin(os.tmpdir(), `${archiveName}-${Date.now()}.zip`)
      fs.copyFileSync(urlJoin(dirname, `../test-assets/${archiveName}.zip`), testFilePath)

      const got = await unzipGithubArchive({
        archiveName: archiveName,
        zipFilePath: testFilePath,
        existingAssets_MUTABLE: [],
      })

      expect(got.projectContents).toEqual(TestProjectUtopiaGithubMain)
      expect(got.assetsToUpload).toHaveLength(4)
      expect(got.assetsToUpload.map((a) => a.path)).toEqual([
        'assets/Curaçao.png',
        'assets/deer.JPG',
        'assets/lanterns.png',
        'assets/pyramids.png',
      ])
    })
  })
})
