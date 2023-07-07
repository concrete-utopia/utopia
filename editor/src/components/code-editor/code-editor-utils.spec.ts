import type { CursorPosition } from './code-editor-utils'
import { cursorPositionToRawOffset, rawOffsetToCursorPosition } from './code-editor-utils'

const sampleCode = `import {
  Ellipse,
  HelperFunctions,
  Image,
  Rectangle,
  Text,
  View
} from "utopia-api";`

describe('Converting Monaco Line Column to Prettier Cursor Offset', () => {
  it('Is correct for the start', () => {
    const cursorPosition: CursorPosition = {
      line: 1,
      column: 1,
    }
    const expected = 0
    const actual = cursorPositionToRawOffset(sampleCode, cursorPosition)
    expect(actual).toEqual(expected)
  })

  it('Is correct for the end', () => {
    const cursorPosition: CursorPosition = {
      line: 8,
      column: 21,
    }
    const expected = 96
    const actual = cursorPositionToRawOffset(sampleCode, cursorPosition)
    expect(actual).toEqual(expected)
  })

  it('Is correct for a point in the middle', () => {
    const cursorPosition: CursorPosition = {
      line: 5,
      column: 12,
    }
    const expected = 59
    const actual = cursorPositionToRawOffset(sampleCode, cursorPosition)
    expect(actual).toEqual(expected)
  })
})

describe('Converting Prettier Cursor Offset to Monaco Line Column', () => {
  it('Is correct for the start', () => {
    const raw = 0
    const expected: CursorPosition = {
      line: 1,
      column: 1,
    }
    const actual = rawOffsetToCursorPosition(sampleCode, raw)
    expect(actual).toEqual(expected)
  })

  it('Is correct for the end', () => {
    const raw = 96
    const expected: CursorPosition = {
      line: 8,
      column: 21,
    }
    const actual = rawOffsetToCursorPosition(sampleCode, raw)
    expect(actual).toEqual(expected)
  })

  it('Is correct for a point in the middle', () => {
    const raw = 59
    const expected: CursorPosition = {
      line: 5,
      column: 12,
    }
    const actual = rawOffsetToCursorPosition(sampleCode, raw)
    expect(actual).toEqual(expected)
  })
})
