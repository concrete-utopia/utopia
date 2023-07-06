import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file.test-utils'
import type { LocalRectangle } from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import type { SimplePinsInfo } from './inspector.test-utils'
import {
  SimpleRect,
  TLWHSimplePins,
  pinsInfoForPins,
  frameForPins,
  TLBRSimplePins,
} from './inspector.test-utils'
import type { ElementFrameInfo } from './layout-property-path-hooks'
import { changePin, PinsInfo } from './layout-property-path-hooks'

function frameInfoForPins(
  pins: SimplePinsInfo,
  localFrame: LocalRectangle = SimpleRect,
  parentFrame: LocalRectangle = SimpleRect,
): ElementFrameInfo {
  return {
    path: EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa']),
    frame: frameForPins(pins),
    localFrame,
    parentFrame,
  }
}

describe('changePin', () => {
  it('Toggles the pin type if clicking an already set pin', () => {
    const pins = TLWHSimplePins
    const { pinsToSet, pinsToUnset } = changePin(
      'width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'left',
      'top',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Toggles the pin type if clicking an already set pin when that pin is also the last set', () => {
    const pins = TLWHSimplePins
    const { pinsToSet, pinsToUnset } = changePin(
      'width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'width',
      'top',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual('100%')

    expect(pinsToUnset.length).toEqual(0)
  })

  it('Retains the last set pin if clicking a new pin', () => {
    const pins = TLBRSimplePins
    const { pinsToSet, pinsToUnset } = changePin(
      'width',
      pinsInfoForPins(pins),
      [frameInfoForPins(pins)],
      'left',
      'top',
    )

    expect(pinsToSet.length).toEqual(1)
    expect(pinsToSet[0]?.pin).toEqual('width')
    expect(pinsToSet[0]?.value).toEqual(SimpleRect.width)

    expect(pinsToUnset.length).toEqual(1)
    expect(pinsToUnset[0]?.pin).toEqual('right')
  })
})
