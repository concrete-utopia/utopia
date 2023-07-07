import type { FilenameParts } from './images'
import { parseMultiplier, filenameFromParts, getFilenameParts } from './images'

describe('image related helper functions', () => {
  it('parseMultiplier', () => {
    const inputs: Array<[string, number | null]> = [
      ['2x', 2],
      ['3x', 3],
      ['22x', 22],
      ['0x', 0],
      ['3241x', 3241],
      ['0xxx', null],
      ['3241_x', null],
      ['32_41x', null],
    ]

    for (const [input, result] of inputs) {
      expect(parseMultiplier(input)).toEqual(result)
    }
  })

  it('print filename object', () => {
    const filenames: Array<[FilenameParts, string]> = [
      [{ filename: 'stuff', extension: 'png' }, 'stuff.png'],
      [{ filename: 'stuff', extension: 'png', multiplier: 2 }, 'stuff@2x.png'],
      [
        { filename: 'stuff', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
        'stuff_2@2x.png',
      ],
    ]

    for (const [input, result] of filenames) {
      const parsed = filenameFromParts(input)
      expect(parsed).toEqual(result)
    }
  })

  it('parse image filenames', () => {
    const filenames: Array<[string, FilenameParts]> = [
      ['stuff.png', { filename: 'stuff', extension: 'png' }],
      ['stuff@2x.png', { filename: 'stuff', extension: 'png', multiplier: 2 }],
      [
        'stuff_2@2x.png',
        { filename: 'stuff', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
      ],
      [
        'stuff@3_2@2x.png',
        { filename: 'stuff@3', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
      ],
    ]

    for (const [input, result] of filenames) {
      const parsed = getFilenameParts(input)
      expect(parsed).toEqual(result)
    }
  })

  it('parse image filenames, off the happy path', () => {
    const filenames: Array<[FilenameParts, string]> = [
      [
        { filename: 'stuff@3', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
        'stuff@3_2@2x.png',
      ],
      [
        { filename: 'stuff_3@3', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
        'stuff_3@3_2@2x.png',
      ],
      [
        { filename: 'stuff_3@@@3', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
        'stuff_3@@@3_2@2x.png',
      ],
      [
        {
          filename: 'stuff_3@3_2.jpeg',
          extension: 'png',
          multiplier: 3,
        },
        'stuff_3@3_2.jpeg@3x.png',
      ],
    ]

    for (const [result, input] of filenames) {
      const parsed = getFilenameParts(input)
      expect(parsed).toEqual(result)
    }
  })

  it('parse image filenames, with full path', () => {
    const filenames: Array<[FilenameParts, string]> = [
      [
        {
          filename: '/Users/user/images/stuff@3',
          extension: 'png',
          multiplier: 2,
          deduplicationSeqNumber: 2,
        },
        '/Users/user/images/stuff@3_2@2x.png',
      ],
      [
        {
          filename: '/Users/user/images/res@3x/stuff_3@3',
          extension: 'png',
          multiplier: 2,
          deduplicationSeqNumber: 2,
        },
        '/Users/user/images/res@3x/stuff_3@3_2@2x.png',
      ],
      [
        {
          filename: '/Users/user/images/res@3x_2/stuff_3@@@3x',
          extension: 'png',
          multiplier: 2,
          deduplicationSeqNumber: 2,
        },
        '/Users/user/images/res@3x_2/stuff_3@@@3x_2@2x.png',
      ],
    ]

    for (const [result, input] of filenames) {
      const parsed = getFilenameParts(input)
      expect(parsed).toEqual(result)
    }
  })

  it('print-parse image filenames', () => {
    const filenames: Array<FilenameParts> = [
      { filename: 'stuff', extension: 'png' },
      { filename: 'stuff', extension: 'png', multiplier: 2 },
      { filename: 'stuff', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
    ]

    for (const filename of filenames) {
      const parsed = getFilenameParts(filenameFromParts(filename))
      expect(parsed).toEqual(filename)
    }
  })
})
