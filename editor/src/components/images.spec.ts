import {
  parseDedupeId,
  parseMultiplier,
  ImageFilenameParts,
  filenameFromParts,
  getImageFilenameParts,
} from './images'

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
      const parsed = parseMultiplier(input)
      if (parsed !== result) {
        // console.log(input, parsed)
      }
      expect(parsed).toEqual(result)
    }
  })

  it('parseDedupeId', () => {
    const inputs: Array<[string, number | null]> = [
      ['_2', 2],
      ['_3', 3],
      ['_22', 22],
      ['_0', 0],
      ['_3241', 3241],
      ['___0', null],
      ['_3241x', null],
      ['x_32', null],
    ]

    for (const [input, result] of inputs) {
      const parsed = parseDedupeId(input)
      if (parsed !== result) {
        // console.log(input, parsed)
      }
      expect(parsed).toEqual(result)
    }
  })

  it('print filename object', () => {
    const filenames: Array<[ImageFilenameParts, string]> = [
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
    const filenames: Array<[ImageFilenameParts, string]> = [
      [{ filename: 'stuff', extension: 'png' }, 'stuff.png'],
      [{ filename: 'stuff', extension: 'png', multiplier: 2 }, 'stuff@2x.png'],
      [
        { filename: 'stuff', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
        'stuff_2@2x.png',
      ],
    ]

    for (const [result, input] of filenames) {
      const parsed = getImageFilenameParts(input)
      expect(parsed).toEqual(result)
    }
  })

  it('print-parse image filenames', () => {
    const filenames: Array<ImageFilenameParts> = [
      { filename: 'stuff', extension: 'png' },
      { filename: 'stuff', extension: 'png', multiplier: 2 },
      { filename: 'stuff', extension: 'png', multiplier: 2, deduplicationSeqNumber: 2 },
    ]

    for (const filename of filenames) {
      const parsed = getImageFilenameParts(filenameFromParts(filename))
      expect(parsed).toEqual(filename)
    }
  })
})
