import { Frame, FramePin } from 'utopia-api'
import { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file'
import { LocalRectangle, localRectangle } from '../../../core/shared/math-utils'
import * as TP from '../../../core/shared/template-path'
import { testInspectorInfo } from './inspector.test-utils'
import { changePin, ElementFrameInfo, PinsInfo } from './layout-property-path-hooks'
import { mapValues } from '../../../core/shared/object-utils'
import { CSSNumber } from './css-utils'

type SimplePinsInfo = { [key in LayoutPinnedProp]: CSSNumber | undefined }

function pinsInfoForPins(pins: SimplePinsInfo): PinsInfo {
  return mapValues((pin) => testInspectorInfo(pin), pins) as PinsInfo
}

function frameForPins(pins: SimplePinsInfo): Frame {
  return {
    left: pins.PinnedLeft?.value,
    centerX: pins.PinnedCenterX?.value,
    right: pins.PinnedRight?.value,
    width: pins.Width?.value,
    top: pins.PinnedTop?.value,
    centerY: pins.PinnedCenterY?.value,
    bottom: pins.PinnedBottom?.value,
    height: pins.Height?.value,
  }
}

const SimpleRect: LocalRectangle = localRectangle({
  x: 10,
  y: 10,
  width: 100,
  height: 100,
})

function frameInfoForPins(
  pins: SimplePinsInfo,
  localFrame: LocalRectangle = SimpleRect,
  parentFrame: LocalRectangle = SimpleRect,
): ElementFrameInfo {
  return {
    path: TP.instancePath(ScenePathForTestUiJsFile.sceneElementPath, ['aaa']),
    frame: frameForPins(pins),
    localFrame,
    parentFrame,
  }
}

describe('changePin', () => {
  const TLWH: SimplePinsInfo = {
    PinnedLeft: {
      value: SimpleRect.x,
      unit: null,
    },
    Width: { value: SimpleRect.width, unit: null },
    PinnedTop: { value: SimpleRect.y, unit: null },
    Height: { value: SimpleRect.height, unit: null },
    PinnedBottom: undefined,
    PinnedRight: undefined,
    PinnedCenterX: undefined,
    PinnedCenterY: undefined,
  }

  const TLBR: SimplePinsInfo = {
    PinnedLeft: { value: SimpleRect.x, unit: null },
    Width: undefined,
    PinnedTop: { value: SimpleRect.y, unit: null },
    Height: undefined,
    PinnedBottom: { value: SimpleRect.y + SimpleRect.height, unit: null },
    PinnedRight: { value: SimpleRect.x + SimpleRect.width, unit: null },
    PinnedCenterX: undefined,
    PinnedCenterY: undefined,
  }

  const CxCyWH: SimplePinsInfo = {
    PinnedLeft: undefined,
    Width: { value: SimpleRect.width, unit: null },
    PinnedTop: undefined,
    Height: { value: SimpleRect.height, unit: null },
    PinnedBottom: undefined,
    PinnedRight: undefined,
    PinnedCenterX: { value: SimpleRect.x, unit: null }, // Offset by 10 since both parent and element frames are the same width
    PinnedCenterY: { value: SimpleRect.y, unit: null }, // Offset by 10 since both parent and element frames are the same height
  }

  it('Toggles the pin type if clicking an already set pin', () => {
    const pins = TLWH
    const { pinsToSet, pinsToUnset } = changePin(
      'Width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'PinnedLeft',
      'PinnedTop',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Toggles the pin type if clicking an already set pin when that pin is also the last set', () => {
    const pins = TLWH
    const { pinsToSet, pinsToUnset } = changePin(
      'Width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'Width',
      'PinnedTop',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Retains the last set pin if clicking a new pin', () => {
    const pins = TLBR
    const { pinsToSet, pinsToUnset } = changePin(
      'Width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'PinnedLeft',
      'PinnedTop',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual(SimpleRect.width)

    expect(pinsToUnset.length).toEqual(1)
    expect(pinsToUnset[0]?.pin).toEqual('right')
  })

  it('Enables the width pin when setting the CX pin', () => {
    const pins = TLBR
    const { pinsToSet, pinsToUnset } = changePin(
      'PinnedCenterX',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'PinnedLeft',
      'PinnedTop',
    )

    expect(pinsToSet.length).toEqual(2)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual(SimpleRect.width)
    expect(pinsToSet[1]?.pin).toEqual('centerX')
    expect(pinsToSet[1]?.value).toEqual(SimpleRect.x)

    expect(pinsToUnset.length).toEqual(2)
    expect(pinsToUnset[0]?.pin).toEqual('left')
    expect(pinsToUnset[1]?.pin).toEqual('right')
  })

  it('Enables the height pin when setting the CY pin', () => {
    const pins = TLBR
    const { pinsToSet, pinsToUnset } = changePin(
      'PinnedCenterY',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'PinnedLeft',
      'PinnedTop',
    )

    expect(pinsToSet.length).toEqual(2)
    expect(pinsToSet[0]?.pin).toEqual('height')
    expect(pinsToSet[0]?.value).toEqual(SimpleRect.height)
    expect(pinsToSet[1]?.pin).toEqual('centerY')
    expect(pinsToSet[1]?.value).toEqual(SimpleRect.y)

    expect(pinsToUnset.length).toEqual(2)
    expect(pinsToUnset[0]?.pin).toEqual('top')
    expect(pinsToUnset[1]?.pin).toEqual('bottom')
  })

  it('Retains the width pin when the CX pin is last set and selecting a new pin', () => {
    const pins = CxCyWH
    const { pinsToSet, pinsToUnset } = changePin(
      'PinnedLeft',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'PinnedCenterX',
      'Height',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('left')
    expect(pinsToSet[0]?.value).toEqual(SimpleRect.x)

    expect(pinsToUnset.length).toEqual(1)
    expect(pinsToUnset[0]?.pin).toEqual('centerX')
  })

  it('Retains the height pin when the CY pin is last set and selecting a new pin', () => {
    const pins = CxCyWH
    const { pinsToSet, pinsToUnset } = changePin(
      'PinnedTop',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'Width',
      'PinnedCenterY',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('top')
    expect(pinsToSet[0]?.value).toEqual(SimpleRect.y)

    expect(pinsToUnset.length).toEqual(1)
    expect(pinsToUnset[0]?.pin).toEqual('centerY')
  })

  it('Toggles the width pin when the CX pin is last set and selecting the width pin', () => {
    const pins = CxCyWH
    const { pinsToSet, pinsToUnset } = changePin(
      'Width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'PinnedCenterX',
      'Height',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Toggles the width pin when the width is last set, CX pin is set, and selecting the width pin', () => {
    const pins = CxCyWH
    const { pinsToSet, pinsToUnset } = changePin(
      'Width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'Width',
      'Height',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Toggles the height pin when the CY pin is last set and selecting the height pin', () => {
    const pins = CxCyWH
    const { pinsToSet, pinsToUnset } = changePin(
      'Height',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'Width',
      'PinnedCenterY',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('height')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Toggles the height pin when the height pin is last set, CY pin is set, and selecting the height pin', () => {
    const pins = CxCyWH
    const { pinsToSet, pinsToUnset } = changePin(
      'Height',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'Width',
      'Height',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('height')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })
})
