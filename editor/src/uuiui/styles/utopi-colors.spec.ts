import { createUtopiColor } from './utopi-color-helpers'
import { blue } from './utopi-colors'

describe('UtopiColors', () => {
  it('createUtopiColor consumes any color formats Chroma eats', () => {
    const colorFromHex = createUtopiColor('#007AFFFF', 'blue', 'blue')
    const colorFromRgba = createUtopiColor('rgba(0,122,255,1)', 'blue', 'blue')
    expect(colorFromHex.value).toEqual(colorFromRgba.value)
  })

  it('the format of .value is the rgba css string', () => {
    const colorFromHex = createUtopiColor('#007AFFFF', 'blue', 'blue')
    expect(colorFromHex.value).toEqual('rgba(0,122,255,1)')
  })

  it('shade 100 is identity function', () => {
    const babyBlue = blue
    expect(babyBlue.value).toEqual(babyBlue.shade(100).value)
    expect(babyBlue.shade(100).value).toEqual(
      babyBlue
        .shade(100)
        .shade(100)
        .shade(100).value,
    )
  })

  it('shade 0 is white', () => {
    const babyBlue = blue
    expect('rgba(255,255,255,1)').toEqual(babyBlue.shade(0).value)
  })

  it('shade 200 is black!', () => {
    const babyBlue = blue
    expect('rgba(0,0,0,1)').toEqual(babyBlue.shade(200).value)
  })

  it('opacity 100 is identity', () => {
    const babyBlueAlpha = createUtopiColor('#007AFFFF', 'blue', 'blue')
    expect(babyBlueAlpha.value).toEqual(babyBlueAlpha.o(100).value)
    expect(babyBlueAlpha.o(100).value).toEqual(
      babyBlueAlpha
        .o(100)
        .o(100)
        .o(100).value,
    )
  })

  it('opacity 0 is alpha channel 0', () => {
    const babyBlueAlpha = createUtopiColor('rgba(0,122,255,1)', 'blue', 'blue')
    expect('rgba(0,122,255,0)').toEqual(babyBlueAlpha.o(0).value)
  })

  it('call order of opacity and shade is not important', () => {
    const babyBlue = createUtopiColor('#007AFFFF', 'blue', 'blue')
    expect(babyBlue.shade(50).o(50)).toEqual(babyBlue.o(50).shade(50))
  })

  const TEST_SIZE = 10000

  xit('test createUtopiColor performance', () => {
    const startTime = performance.now()
    for (let i = 0; i < TEST_SIZE; i++) {
      createUtopiColor('#ff00ff', 'pink', 'punk')
    }
    const endTime = performance.now()
    console.info('CREATE UTOPICOLORS PERFORMANCE MEASURE', (endTime - startTime) / TEST_SIZE)
  })

  xit('test shade performance', () => {
    const babyBlue = blue
    const startTime = performance.now()
    for (let i = 0; i < TEST_SIZE; i++) {
      babyBlue.shade(99).value
    }
    const endTime = performance.now()
    console.info('SHADE PERFORMANCE MEASURE', (endTime - startTime) / TEST_SIZE)
  })

  xit('test opacity performance', () => {
    const babyBlue = blue
    const startTime = performance.now()
    for (let i = 0; i < TEST_SIZE; i++) {
      babyBlue.o(5).value
    }
    const endTime = performance.now()
    console.info('OPACITY PERFORMANCE MEASURE', (endTime - startTime) / TEST_SIZE)
  })
})
