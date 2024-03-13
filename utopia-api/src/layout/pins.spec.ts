import type { PinFrameProps } from './frame'
import { convertPinsToStyleProps } from './pins'

describe('Pins to CSS Conversion', () => {
  it('translates a simple top-left-width-height', () => {
    const frame: PinFrameProps = { top: 13, left: 17, width: 99, height: 101 }
    const styleProp = convertPinsToStyleProps(frame)
    expect(styleProp).toEqual(frame)
  })

  it('translates a simple right-bottom-width-height', () => {
    const frame: PinFrameProps = { right: 13, bottom: 17, width: 99, height: 101 }
    const styleProp = convertPinsToStyleProps(frame)
    expect(styleProp).toEqual(frame)
  })

  it('translates a simple top-leftright-bottom', () => {
    const frame: PinFrameProps = { top: 13, left: 17, right: 99, bottom: 101 }
    const styleProp = convertPinsToStyleProps(frame)
    expect(styleProp).toEqual(frame)
  })

  it('turns a centerX, centerY, width, height into top left width height with calc', () => {
    const frame: PinFrameProps = { centerX: 19, centerY: 21, width: 82, height: 102 }
    const styleProp = convertPinsToStyleProps(frame)
    const expectedStyle: React.CSSProperties = {
      left: 'calc((50% + 19px) - (82px / 2))',
      top: 'calc((50% + 21px) - (102px / 2))',
      width: 82,
      height: 102,
    }
    expect(styleProp).toEqual(expectedStyle)
  })

  it('turns a centerX, centerY, right, bottom into bottom left right top with calc', () => {
    const frame: PinFrameProps = { centerX: 20, centerY: 40, right: 35, bottom: 70 }
    const styleProp = convertPinsToStyleProps(frame)
    const expectedStyle: React.CSSProperties = {
      bottom: 70,
      left: 'calc(35px - calc(50% - 20px - 35px) * 2)',
      right: 35,
      top: 'calc(70px - calc(50% - 40px - 70px) * 2)',
    }
    expect(styleProp).toEqual(expectedStyle)
  })
})
