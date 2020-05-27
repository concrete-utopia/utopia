import { parsePrintedCanvasMetadata } from './canvas-metadata-parser'
import { left, right } from '../../shared/either'
import { PrintedCanvasMetadata } from '../../shared/project-file-types'
import { LayoutSystem } from 'utopia-api'
import {
  descriptionParseError,
  objectFieldNotPresentParseError,
} from '../../../utils/value-parser-utils'

const printedCanvasMetadata1: PrintedCanvasMetadata = {
  scenes: [
    {
      uid: 'scene-0',
      component: 'App',
      props: {},
      frame: { left: 0, top: 0, width: 268, height: 529 },
      container: { layoutSystem: LayoutSystem.PinSystem },
    },
    {
      uid: 'scene-1',
      component: 'Inner',
      props: { layout: { top: 0, left: 0, right: 0, bottom: 0 } },
      frame: { left: 335, top: 17, width: 278, height: 329 },
      container: { layoutSystem: LayoutSystem.PinSystem },
      label: 'Scene 1',
    },
  ],
  elementMetadata: {},
}

const printedCanvasMetadata2: PrintedCanvasMetadata = {
  scenes: [],
  elementMetadata: {},
}

describe('parsePrintedCanvasMetadata', () => {
  it('returns a left for something which is not a PrintedCanvasMetadata', () => {
    expect(parsePrintedCanvasMetadata(9)).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
    expect(parsePrintedCanvasMetadata(true)).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
    expect(parsePrintedCanvasMetadata({})).toEqual(left(objectFieldNotPresentParseError('scenes')))
    expect(parsePrintedCanvasMetadata([])).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
    expect(parsePrintedCanvasMetadata(null)).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
    expect(parsePrintedCanvasMetadata(undefined)).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
  })
  it('returns a right for something which is a valid value', () => {
    expect(parsePrintedCanvasMetadata(printedCanvasMetadata1)).toEqual(
      right(printedCanvasMetadata1),
    )
    expect(parsePrintedCanvasMetadata(printedCanvasMetadata2)).toEqual(
      right(printedCanvasMetadata2),
    )
  })
})
