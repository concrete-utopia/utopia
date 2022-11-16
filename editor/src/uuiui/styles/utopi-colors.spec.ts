import { UtopiColor } from './utopi-color-helpers'

const { createUtopiColor } = UtopiColor

describe('UtopiColors', () => {
  it('createUtopiColor consumes any color formats Chroma eats', () => {
    const colorFromHex = createUtopiColor('#007AFFFF')
    const colorFromRgba = createUtopiColor('rgba(0,122,255,1)')
    expect(colorFromHex.cssValue).toEqual(colorFromRgba.cssValue)
  })

  it('the format of .cssValue is the rgba css string', () => {
    const colorFromHex = createUtopiColor('#007AFFFF')
    expect(colorFromHex.cssValue).toEqual('rgba(0,122,255,1)')
  })

  it('opacity 100 is identity', () => {
    const babyBlueAlpha = createUtopiColor('#007AFFFF')
    expect(babyBlueAlpha.cssValue).toEqual(babyBlueAlpha.o(100).cssValue)
    expect(babyBlueAlpha.o(100).cssValue).toEqual(babyBlueAlpha.o(100).o(100).o(100).cssValue)
  })

  it('opacity 0 is alpha channel 0', () => {
    const babyBlueAlpha = createUtopiColor('rgba(0,122,255,1)')
    expect('rgba(0,122,255,0)').toEqual(babyBlueAlpha.o(0).cssValue)
  })

  const TEST_SIZE = 10000

  xit('test createUtopiColor performance', () => {
    const startTime = performance.now()
    for (let i = 0; i < TEST_SIZE; i++) {
      createUtopiColor('#ff00ff')
    }
    const endTime = performance.now()
    console.info('CREATE UTOPICOLORS PERFORMANCE MEASURE', (endTime - startTime) / TEST_SIZE)
  })

  xit('test opacity performance', () => {
    const babyBlue = createUtopiColor('#007AFF')
    const startTime = performance.now()
    for (let i = 0; i < TEST_SIZE; i++) {
      babyBlue.o(5).cssValue
    }
    const endTime = performance.now()
    console.info('OPACITY PERFORMANCE MEASURE', (endTime - startTime) / TEST_SIZE)
  })
})
