import { Frame, FramePin } from 'utopia-api'
import { Utils } from 'uuiui-deps'
import { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file'
import { LocalRectangle, localRectangle } from '../../../core/shared/math-utils'
import * as TP from '../../../core/shared/template-path'
import { testInspectorInfo } from './inspector.test-utils'
import { changePin, ElementFrameInfo, PinsInfo } from './layout-property-path-hooks'

type SimplePinsInfo = { [key in LayoutPinnedProp]: FramePin | undefined }

function pinsInfoForPins(pins: SimplePinsInfo): PinsInfo {
  return Utils.mapValues((pin) => testInspectorInfo(pin), pins) as PinsInfo
}

function frameForPins(pins: SimplePinsInfo): Frame {
  return {
    left: pins.PinnedLeft,
    centerX: pins.PinnedCenterX,
    right: pins.PinnedRight,
    width: pins.Width,
    top: pins.PinnedTop,
    centerY: pins.PinnedCenterY,
    bottom: pins.PinnedBottom,
    height: pins.Height,
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
    PinnedLeft: SimpleRect.x,
    Width: SimpleRect.width,
    PinnedTop: SimpleRect.y,
    Height: SimpleRect.height,
    PinnedBottom: undefined,
    PinnedRight: undefined,
    PinnedCenterX: undefined,
    PinnedCenterY: undefined,
  }

  const TLBR: SimplePinsInfo = {
    PinnedLeft: SimpleRect.x,
    Width: undefined,
    PinnedTop: SimpleRect.y,
    Height: undefined,
    PinnedBottom: SimpleRect.y + SimpleRect.height,
    PinnedRight: SimpleRect.x + SimpleRect.width,
    PinnedCenterX: undefined,
    PinnedCenterY: undefined,
  }

  const CxCyWH: SimplePinsInfo = {
    PinnedLeft: undefined,
    Width: SimpleRect.width,
    PinnedTop: undefined,
    Height: SimpleRect.height,
    PinnedBottom: undefined,
    PinnedRight: undefined,
    PinnedCenterX: SimpleRect.x, // Offset by 10 since both parent and element frames are the same width
    PinnedCenterY: SimpleRect.y, // Offset by 10 since both parent and element frames are the same height
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
