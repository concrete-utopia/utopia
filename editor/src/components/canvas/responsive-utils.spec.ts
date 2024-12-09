import * as csstree from 'css-tree'
import { mediaQueryToScreenSize, selectValueByBreakpoint } from './responsive-utils'
import type { ScreenSize, MediaQuery } from './responsive-types'
import { extractScreenSizeFromCss } from './responsive-utils'
import type { StyleMediaSizeModifier } from './canvas-types'

describe('extractScreenSizeFromCss', () => {
  it('extracts screen size from simple media query', () => {
    const css = '@media (min-width: 100px) and (max-width: 500px)'
    const result = extractScreenSizeFromCss(css)
    expect(result).toEqual({
      min: { value: 100, unit: 'px' },
      max: { value: 500, unit: 'px' },
    })
  })

  it('returns null for invalid media query', () => {
    const css = 'not-a-media-query'
    const result = extractScreenSizeFromCss(css)
    expect(result).toBeNull()
  })

  it('uses cache for repeated calls with same CSS', () => {
    const css = '@media (min-width: 100px)'

    // First call
    const result1 = extractScreenSizeFromCss(css)
    // Second call - should return same object reference
    const result2 = extractScreenSizeFromCss(css)

    expect(result1).toBe(result2) // Use toBe for reference equality
    expect(result1).toEqual({
      min: { value: 100, unit: 'px' },
    })
  })

  it('caches null results', () => {
    const css = 'invalid-css'

    // First call
    const result1 = extractScreenSizeFromCss(css)
    // Second call - should return same null reference
    const result2 = extractScreenSizeFromCss(css)

    expect(result1).toBe(result2)
    expect(result1).toBeNull()
  })

  it('handles different CSS strings independently in cache', () => {
    const css1 = '@media (min-width: 100px)'
    const css2 = '@media (max-width: 500px)'

    // First string
    const result1a = extractScreenSizeFromCss(css1)
    const result1b = extractScreenSizeFromCss(css1)
    expect(result1a).toBe(result1b)
    expect(result1a).toEqual({
      min: { value: 100, unit: 'px' },
    })

    // Second string
    const result2a = extractScreenSizeFromCss(css2)
    const result2b = extractScreenSizeFromCss(css2)
    expect(result2a).toBe(result2b)
    expect(result2a).toEqual({
      max: { value: 500, unit: 'px' },
    })

    // Different strings should have different references
    expect(result1a).not.toBe(result2a)
  })
})

describe('selectValueByBreakpoint', () => {
  const variants: { value: string; modifiers?: StyleMediaSizeModifier[] }[] = [
    {
      value: 'b',
      modifiers: [{ type: 'media-size', size: { min: { value: 200, unit: 'px' } } }],
    },
    {
      value: 'a',
      modifiers: [{ type: 'media-size', size: { min: { value: 100, unit: 'px' } } }],
    },
    {
      value: 'c',
      modifiers: [{ type: 'media-size', size: { min: { value: 20, unit: 'em' } } }],
    },
    { value: 'd' },
  ]
  it('selects the correct value', () => {
    expect(selectValueByBreakpoint(variants, 150)).toEqual({
      value: 'a',
      modifiers: [{ type: 'media-size', size: { min: { value: 100, unit: 'px' } } }],
    })
  })
  it('select the closest value', () => {
    expect(selectValueByBreakpoint(variants, 250)).toEqual({
      value: 'b',
      modifiers: [{ type: 'media-size', size: { min: { value: 200, unit: 'px' } } }],
    })
  })
  it('converts em to px', () => {
    expect(selectValueByBreakpoint(variants, 350)).toEqual({
      value: 'c',
      modifiers: [{ type: 'media-size', size: { min: { value: 20, unit: 'em' } } }],
    })
  })
  it('selects the first value if no breakpoint is matched', () => {
    expect(selectValueByBreakpoint(variants, 50)).toEqual({ value: 'd' })
  })
  it('selects null if no matching breakpoint and no default value', () => {
    expect(selectValueByBreakpoint(variants.slice(0, 2), 50)).toBeNull()
  })
  it('selects default value if no media modifiers', () => {
    expect(
      selectValueByBreakpoint(
        [
          {
            value: 'a',
            modifiers: [{ type: 'hover' }],
          },
          { value: 'c' },
        ],
        50,
      ),
    ).toEqual({ value: 'c' })
  })
})

describe('mediaQueryToScreenSize', () => {
  it('converts simple screen size queries', () => {
    const testCases: { input: string; expected: ScreenSize }[] = [
      {
        input: '@media (100px <width < 500px)',
        expected: { min: { value: 100, unit: 'px' }, max: { value: 500, unit: 'px' } },
      },
      {
        input: '@media (min-width: 100px) and (max-width: 500px)',
        expected: { min: { value: 100, unit: 'px' }, max: { value: 500, unit: 'px' } },
      },
      {
        input: '@media screen and (min-width: 100px)',
        expected: { min: { value: 100, unit: 'px' } },
      },
      {
        input: '@media (100px < width) and (max-width: 500px)',
        expected: { min: { value: 100, unit: 'px' }, max: { value: 500, unit: 'px' } },
      },
      {
        input: '@media (width > 100px)',
        expected: { min: { value: 100, unit: 'px' } },
      },
    ]
    testCases.forEach((testCase) => {
      csstree.walk(csstree.parse(testCase.input), (node) => {
        if (node.type === 'MediaQuery') {
          const result = mediaQueryToScreenSize(node as unknown as MediaQuery)
          expect(result).toEqual(testCase.expected)
        }
      })
    })
  })
})
