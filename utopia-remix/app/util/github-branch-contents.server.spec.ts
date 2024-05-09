import urlJoin from 'url-join'
import type { ProjectContentDirectory } from '../types'
import { projectContentDirectory, type ExistingAsset } from '../types'
import type { AssetToUpload, UnzipEntry } from './github-branch-contents'
import {
  populateDirectories,
  populateEntryContents,
  processEntry,
  shouldUploadAsset,
  unzipGithubArchive,
} from './github-branch-contents'
import * as fs from 'fs'
import * as os from 'os'
import path from 'path'

describe('Github clone', () => {
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

      const target = populateDirectories(root, 'some/file/here.png')

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

  describe('populateEntryContents', () => {
    it('populates the leaf for a directory entry', async () => {
      let root = projectContentDirectory('root')
      const target = populateDirectories(root, 'some/dir/here')
      await populateEntryContents(
        'some/dir/here',
        { type: 'Directory' } as UnzipEntry,
        target,
        [],
        [],
      )
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
      const target = populateDirectories(root, 'some/file/here.json')
      await populateEntryContents(
        'some/file/here.json',
        {
          type: 'File',
          vars: { uncompressedSize: 123 },
          buffer: async () => Buffer.from(`"hello there"`),
        } as UnzipEntry,
        target,
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
    it('adds a new file to upload if there is a need', async () => {
      let root = projectContentDirectory('root')
      const target = populateDirectories(root, 'some/file/here.png')

      const toUpload: AssetToUpload[] = []

      await populateEntryContents(
        'some/file/here.png',
        {
          type: 'File',
          vars: { uncompressedSize: 123 },
          buffer: async () => Buffer.from(`"hello there"`),
        } as UnzipEntry,
        target,
        [],
        toUpload,
      )

      expect(root.children).toEqual({
        some: {
          children: {
            file: {
              children: {
                'here.png': {
                  content: {
                    base64: null,
                    gitBlobSha: '6ed5c1e4f0c1dc6bd7f3e6a0a52c20585f5420be', // <- this!
                    hash: 0,
                    imageType: null,
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
      expect(toUpload[0].data.length).toEqual(13) // length in bytes of "hello there"
    })
  })

  describe('processEntry', () => {
    it('processes the entry', async () => {
      let root = projectContentDirectory('root')

      await processEntry(
        'the-archive',
        root,
        [],
        [],
      )({
        path: 'the-archive/some/file/here.json',
        type: 'File',
        vars: { uncompressedSize: 123 },
        buffer: async () => Buffer.from(`"hello there"`),
        autodrain: () => {},
      } as UnzipEntry)

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

      const expected = JSON.parse(
        fs.readFileSync(urlJoin(dirname, `../test-assets/${archiveName}.json`)).toString(),
      )

      const got = await unzipGithubArchive({
        archiveName: archiveName,
        zipFilePath: testFilePath,
        existingAssets: [],
      })

      expect(got.projectContents).toEqual(expected)
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
