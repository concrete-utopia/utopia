import type { NormalisedFrame, Frame } from './frame'
import { FramePoint, toNormalisedFrame, valueToUseForPin, zeroIfNegative } from './frame'

const parentFrame: NormalisedFrame = {
  top: 0,
  left: 0,
  width: 200,
  height: 200,
}

const normalisedFrame: NormalisedFrame = {
  top: 20,
  left: 20,
  width: 160,
  height: 160,
}

describe('Pinned numeric layout', () => {
  it('Correctly sets the frame for TLWH', () => {
    const frame: Frame = {
      top: 20,
      left: 20,
      width: 160,
      height: 160,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for TLBR', () => {
    const frame: Frame = {
      top: 20,
      left: 20,
      bottom: 20,
      right: 20,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for TLCxCy', () => {
    const frame: Frame = {
      top: 20,
      left: 20,
      centerX: 0,
      centerY: 0,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for CxCyWH', () => {
    const frame: Frame = {
      centerX: 0,
      centerY: 0,
      width: 160,
      height: 160,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for CxCyBR', () => {
    const frame: Frame = {
      centerX: 0,
      centerY: 0,
      bottom: 20,
      right: 20,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
})

describe('Pinned percentage layout', () => {
  it('Correctly sets the frame for TLWH', () => {
    const frame: Frame = {
      top: '10%',
      left: '10%',
      width: '80%',
      height: '80%',
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for TLBR', () => {
    const frame: Frame = {
      top: '10%',
      left: '10%',
      bottom: '10%',
      right: '10%',
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for TLCxCy', () => {
    const frame: Frame = {
      top: '10%',
      left: '10%',
      centerX: '0%',
      centerY: '0%',
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for CxCyWH', () => {
    const frame: Frame = {
      centerX: '0%',
      centerY: '0%',
      width: '80%',
      height: '80%',
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Correctly sets the frame for CxCyBR', () => {
    const frame: Frame = {
      centerX: '0%',
      centerY: '0%',
      bottom: '10%',
      right: '10%',
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
})

describe('Pinned mixed layout', () => {
  it('Correctly sets the frame for TLWH', () => {
    const frame: Frame = {
      top: '10%',
      left: 20,
      width: '80%',
      height: 160,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    expect(actual).toEqual(normalisedFrame)
  })
  it('Returns zero horizontal values if the horizontal definition is incomplete', () => {
    const frame: Frame = {
      top: 20,
      right: 20,
      height: 160,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    const expected: NormalisedFrame = {
      top: 20,
      left: 0,
      width: 0,
      height: 160,
    }
    expect(actual).toEqual(expected)
  })
  it('Returns zero vertical values if the vertical definition is incomplete', () => {
    const frame: Frame = {
      bottom: 20,
      left: 20,
      width: 160,
    }

    const actual = toNormalisedFrame(frame, parentFrame)
    const expected: NormalisedFrame = {
      top: 0,
      left: 20,
      width: 160,
      height: 0,
    }
    expect(actual).toEqual(expected)
  })
  it('calculates the correct frame for center oriented frames', () => {
    const parentNormalisedFrame: NormalisedFrame = {
      top: 0,
      left: 0,
      width: 380,
      height: 800,
    }
    const centerX = '-5%'
    const centerY = '-25%'
    const right = 270
    const height = 80
    const frame: Frame = {
      centerX: centerX,
      centerY: centerY,
      right: right,
      height: height,
    }
    const parentFrameCenterX = parentNormalisedFrame.width / 2
    const parentFrameCenterY = parentNormalisedFrame.height / 2
    const childCenterX = parentFrameCenterX - parentNormalisedFrame.width * 0.05
    const childRight = parentNormalisedFrame.width - right
    const childWidth = (childRight - childCenterX) * 2
    const childTop = parentFrameCenterY - height / 2 - parentNormalisedFrame.height * 0.25
    const actual = toNormalisedFrame(frame, parentNormalisedFrame)
    const expected: NormalisedFrame = {
      top: zeroIfNegative(childTop),
      left: zeroIfNegative(childRight - childWidth),
      width: zeroIfNegative(childWidth),
      height: zeroIfNegative(height),
    }
    expect(actual).toEqual(expected)
  })
})

describe('valueToUseForPin for numeric pins', () => {
  it('returns the correct Left value', () => {
    const actual = valueToUseForPin(FramePoint.Left, 20, false, parentFrame)
    expect(actual).toEqual(20)
  })
  it('returns the correct CenterX value', () => {
    const actual = valueToUseForPin(FramePoint.CenterX, 100, false, parentFrame)
    expect(actual).toEqual(0)
  })
  it('returns the correct Right value', () => {
    const actual = valueToUseForPin(FramePoint.Right, 180, false, parentFrame)
    expect(actual).toEqual(20)
  })
  it('returns the correct Width value', () => {
    const actual = valueToUseForPin(FramePoint.Width, 160, false, parentFrame)
    expect(actual).toEqual(160)
  })
  it('returns the correct Top value', () => {
    const actual = valueToUseForPin(FramePoint.Top, 20, false, parentFrame)
    expect(actual).toEqual(20)
  })
  it('returns the correct CenterY value', () => {
    const actual = valueToUseForPin(FramePoint.CenterY, 100, false, parentFrame)
    expect(actual).toEqual(0)
  })
  it('returns the correct Bottom value', () => {
    const actual = valueToUseForPin(FramePoint.Bottom, 180, false, parentFrame)
    expect(actual).toEqual(20)
  })
  it('returns the correct Height value', () => {
    const actual = valueToUseForPin(FramePoint.Height, 160, false, parentFrame)
    expect(actual).toEqual(160)
  })
})

describe('valueToUseForPin for percentage pins', () => {
  it('returns the correct Left value', () => {
    const actual = valueToUseForPin(FramePoint.Left, 20, true, parentFrame)
    expect(actual).toEqual('10%')
  })
  it('returns the correct CenterX value', () => {
    const actual = valueToUseForPin(FramePoint.CenterX, 100, true, parentFrame)
    expect(actual).toEqual('0%')
  })
  it('returns the correct Right value', () => {
    const actual = valueToUseForPin(FramePoint.Right, 180, true, parentFrame)
    expect(actual).toEqual('10%')
  })
  it('returns the correct Width value', () => {
    const actual = valueToUseForPin(FramePoint.Width, 160, true, parentFrame)
    expect(actual).toEqual('80%')
  })
  it('returns the correct Top value', () => {
    const actual = valueToUseForPin(FramePoint.Top, 20, true, parentFrame)
    expect(actual).toEqual('10%')
  })
  it('returns the correct CenterY value', () => {
    const actual = valueToUseForPin(FramePoint.CenterY, 100, true, parentFrame)
    expect(actual).toEqual('0%')
  })
  it('returns the correct Bottom value', () => {
    const actual = valueToUseForPin(FramePoint.Bottom, 180, true, parentFrame)
    expect(actual).toEqual('10%')
  })
  it('returns the correct Height value', () => {
    const actual = valueToUseForPin(FramePoint.Height, 160, true, parentFrame)
    expect(actual).toEqual('80%')
  })
})
