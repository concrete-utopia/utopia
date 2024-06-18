import {
  updateStrikethroughTextDecoration,
  updateUnderlinedTextDecoration,
} from './text-subsection'

describe('updateUnderlinedTextDecoration', () => {
  it('removes the underline option when it is the only one', () => {
    const actualResult = updateUnderlinedTextDecoration(false, 'underline')
    expect(actualResult).toEqual('none')
  })
  it('removes the underline option when there is none', () => {
    const actualResult = updateUnderlinedTextDecoration(false, 'none')
    expect(actualResult).toEqual('none')
  })
  it('removes the underline option when the value is empty', () => {
    const actualResult = updateUnderlinedTextDecoration(false, '')
    expect(actualResult).toEqual('none')
  })
  it('removes the underline option when the value is something else', () => {
    const actualResult = updateUnderlinedTextDecoration(false, 'line-through')
    expect(actualResult).toEqual('line-through')
  })
  it('removes the underline option when the value is at the end', () => {
    const actualResult = updateUnderlinedTextDecoration(false, 'line-through underline')
    expect(actualResult).toEqual('line-through')
  })
  it('removes the underline option when the value is at the start', () => {
    const actualResult = updateUnderlinedTextDecoration(false, 'underline line-through')
    expect(actualResult).toEqual('line-through')
  })
  it('adds the underline option when the value is none', () => {
    const actualResult = updateUnderlinedTextDecoration(true, 'none')
    expect(actualResult).toEqual('underline')
  })
  it('adds the underline option when the value is underline', () => {
    const actualResult = updateUnderlinedTextDecoration(true, 'underline')
    expect(actualResult).toEqual('underline')
  })
  it('adds the underline option when the value is line-through', () => {
    const actualResult = updateUnderlinedTextDecoration(true, 'line-through')
    expect(actualResult).toEqual('line-through underline')
  })
  it('adds the underline option when the value is already at the start', () => {
    const actualResult = updateUnderlinedTextDecoration(true, 'underline line-through')
    expect(actualResult).toEqual('underline line-through')
  })
  it('adds the underline option when the value is already at the end', () => {
    const actualResult = updateUnderlinedTextDecoration(true, 'line-through underline')
    expect(actualResult).toEqual('line-through underline')
  })
})

describe('updateStrikethroughTextDecoration', () => {
  it('removes the line-through option when it is the only one', () => {
    const actualResult = updateStrikethroughTextDecoration(false, 'line-through')
    expect(actualResult).toEqual('none')
  })
  it('removes the line-through option when there is none', () => {
    const actualResult = updateStrikethroughTextDecoration(false, 'none')
    expect(actualResult).toEqual('none')
  })
  it('removes the line-through option when the value is empty', () => {
    const actualResult = updateStrikethroughTextDecoration(false, '')
    expect(actualResult).toEqual('none')
  })
  it('removes the line-through option when the value is something else', () => {
    const actualResult = updateStrikethroughTextDecoration(false, 'underline')
    expect(actualResult).toEqual('underline')
  })
  it('removes the line-through option when the value is at the end', () => {
    const actualResult = updateStrikethroughTextDecoration(false, 'underline line-through')
    expect(actualResult).toEqual('underline')
  })
  it('removes the line-through option when the value is at the start', () => {
    const actualResult = updateStrikethroughTextDecoration(false, 'line-through underline')
    expect(actualResult).toEqual('underline')
  })
  it('adds the line-through option when the value is none', () => {
    const actualResult = updateStrikethroughTextDecoration(true, 'none')
    expect(actualResult).toEqual('line-through')
  })
  it('adds the line-through option when the value is line-through', () => {
    const actualResult = updateStrikethroughTextDecoration(true, 'line-through')
    expect(actualResult).toEqual('line-through')
  })
  it('adds the line-through option when the value is underline', () => {
    const actualResult = updateStrikethroughTextDecoration(true, 'underline')
    expect(actualResult).toEqual('underline line-through')
  })
  it('adds the line-through option when the value is already at the start', () => {
    const actualResult = updateStrikethroughTextDecoration(true, 'line-through underline')
    expect(actualResult).toEqual('line-through underline')
  })
  it('adds the line-through option when the value is already at the end', () => {
    const actualResult = updateStrikethroughTextDecoration(true, 'underline line-through')
    expect(actualResult).toEqual('underline line-through')
  })
})
