import { adjustRuleScopeImpl } from './tailwind'

describe('adjustRuleScope', () => {
  it('Returns a rule unchanged if no prefix is provided', () => {
    const input = '.container{width:100%}'
    const output = adjustRuleScopeImpl(input, null)
    const expected = input
    expect(output).toEqual(expected)
  })

  it('Returns a rule unchanged if it is a keyframe', () => {
    const input = '@keyframes spin{to{transform:rotate(360deg)}}'
    const output = adjustRuleScopeImpl(input, '#canvas-container')
    const expected = input
    expect(output).toEqual(expected)
  })

  it('Prefixes a regular selector', () => {
    const input = '.container{width:100%}'
    const output = adjustRuleScopeImpl(input, '#canvas-container')
    const expected = '#canvas-container .container{width:100%}'
    expect(output).toEqual(expected)
  })

  it('Handles comma separated selectors', () => {
    const input = '.container,.other-container{width:100%}'
    const output = adjustRuleScopeImpl(input, '#canvas-container')
    const expected = '#canvas-container .container,#canvas-container .other-container{width:100%}'
    expect(output).toEqual(expected)
  })

  it('Replaces :root, html, or head with the provided prefix', () => {
    const rootOutput = adjustRuleScopeImpl(':root{width:100%}', '#canvas-container')
    const htmlOutput = adjustRuleScopeImpl('html{width:100%}', '#canvas-container')
    const headOutput = adjustRuleScopeImpl('head{width:100%}', '#canvas-container')
    const expected = '#canvas-container{width:100%}'
    expect(rootOutput).toEqual(expected)
    expect(htmlOutput).toEqual(expected)
    expect(headOutput).toEqual(expected)
  })

  it('Replaces body with a selector for children of the provided prefix', () => {
    const input = 'body{width:100%}'
    const output = adjustRuleScopeImpl(input, '#canvas-container')
    const expected = '#canvas-container > *{width:100%}'
    expect(output).toEqual(expected)
  })

  it('Handles a media query', () => {
    const input = '@media (min-width:640px){.container{max-width:640px}}'
    const output = adjustRuleScopeImpl(input, '#canvas-container')
    const expected = '@media (min-width:640px){#canvas-container .container{max-width:640px}}'
    expect(output).toEqual(expected)
  })
})
