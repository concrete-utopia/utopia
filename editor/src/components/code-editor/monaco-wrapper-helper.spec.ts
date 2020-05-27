import { OPENING_TAG_REGEX } from './monaco-wrapper-helper'

describe('Opening tag regular expression tests', () => {
  it('Regex matches simple opening tag', () => {
    const content = '<View>'
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).not.toBeNull()
    expect(match).toHaveLength(3)
    expect(match![1]).toEqual('View')
  })
  it('Regex matches opening tag with props', () => {
    const content = `<View onMouseDown={doSomething}} layout={{ layoutSystem: 'pinSystem' }>`
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).not.toBeNull()
    expect(match).toHaveLength(3)
    expect(match![1]).toEqual('View')
  })
  it('Regex matches opening tag with props with => inside', () => {
    const content = `<View onMouseDown={(e) => doSomething(e)}} layout={{ layoutSystem: 'pinSystem' }>`
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).not.toBeNull()
    expect(match).toHaveLength(3)
    expect(match![1]).toEqual('View')
  })
  it('Regex matches simple multiline opening tag', () => {
    const content = ` <View
  >`
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).not.toBeNull()
    expect(match).toHaveLength(3)
    expect(match![1]).toEqual('View')
  })
  it('Regex matches multiline opening tag with props', () => {
    const content = `<View
  onMouseDown={doSomething}}
  layout={{ layoutSystem: 'pinSystem' }>`
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).not.toBeNull()
    expect(match).toHaveLength(3)
    expect(match![1]).toEqual('View')
  })
  it('Regex matches multiline opening tag with props with => inside', () => {
    const content = `<View
  onMouseDown={(e) => doSomething(e)}}
  layout={{ layoutSystem: 'pinSystem' }>`
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).not.toBeNull()
    expect(match).toHaveLength(3)
    expect(match![1]).toEqual('View')
  })
  it('Regex doesnt match self-closing tag', () => {
    const content = '<View />'
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).toBeNull()
  })
  it('Regex doesnt match closing tag', () => {
    const content = '</View>'
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).toBeNull()
  })
  it('Regex doesnt match tag without opening bracket', () => {
    const content = 'View>'
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).toBeNull()
  })
  it('Regex doesnt match => operator inside prop of opening tag', () => {
    const content = '<View onMouseDown={(e) =>'
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).toBeNull()
  })
  it('Regex doesnt match => operator inside prop of multilineopening tag', () => {
    const content = `<View
  onMouseDown={(e) =>`
    const match = content.match(OPENING_TAG_REGEX)
    expect(match).toBeNull()
  })
})
