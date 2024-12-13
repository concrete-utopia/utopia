import * as csstree from 'css-tree'
import { mediaQueryToScreenSize, selectValueByBreakpoint } from './responsive-utils'
import type { ScreenSize, MediaQuery } from './responsive-types'
import { extractScreenSizeFromCss } from './responsive-utils'
import type { CSSVariant, StyleModifier } from './canvas-types'

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
  const variants: CSSVariant<string>[] = [
    {
      value: 'Desktop Value',
      modifiers: [{ type: 'media-size', size: { min: { value: 200, unit: 'px' } } }],
    },
    {
      value: 'Tablet Value',
      modifiers: [{ type: 'media-size', size: { min: { value: 100, unit: 'px' } } }],
    },
    {
      value: 'Extra Large Value',
      modifiers: [{ type: 'media-size', size: { min: { value: 20, unit: 'em' } } }],
    },
    {
      value: 'Ranged Value',
      modifiers: [
        {
          type: 'media-size',
          size: { min: { value: 80, unit: 'px' }, max: { value: 90, unit: 'px' } },
        },
      ],
    },
    {
      value: 'Mobile Value',
      modifiers: [{ type: 'media-size', size: { min: { value: 60, unit: 'px' } } }],
    },
    { value: 'Default Value', modifiers: [] },
  ]
  const tests: { title: string; screenSize: number; expected: string }[] = [
    { title: 'selects the correct value', screenSize: 150, expected: 'Tablet Value' },
    { title: 'select the closest value', screenSize: 250, expected: 'Desktop Value' },
    { title: 'converts em to px', screenSize: 350, expected: 'Extra Large Value' },
    {
      title: 'selects the default value if no breakpoint is matched',
      screenSize: 50,
      expected: 'Default Value',
    },
    {
      title: 'selects the ranged value if the screen size is within the range',
      screenSize: 85,
      expected: 'Ranged Value',
    },
    {
      title: 'selects the mobile value if the screen size is outside the ranged values',
      screenSize: 95,
      expected: 'Mobile Value',
    },
  ] as const

  tests.forEach((test) => {
    it(`${test.title}`, () => {
      expect(selectValueByBreakpoint(variants, test.screenSize)?.value).toEqual(test.expected)
    })
  })

  it('selects null if no matching breakpoint and no default value', () => {
    const largeVariants: CSSVariant<string>[] = [
      {
        value: 'Desktop Value',
        modifiers: [{ type: 'media-size', size: { min: { value: 200, unit: 'px' } } }],
      },
      {
        value: 'Tablet Value',
        modifiers: [{ type: 'media-size', size: { min: { value: 100, unit: 'px' } } }],
      },
    ]
    expect(selectValueByBreakpoint(largeVariants, 50)).toBeNull()
  })
  it('selects default value if no media modifiers', () => {
    const noMediaVariants: CSSVariant<string>[] = [
      {
        value: 'Hover Value',
        modifiers: [{ type: 'hover' }],
      },
      { value: 'Default Value', modifiers: [] },
    ]
    expect(selectValueByBreakpoint(noMediaVariants, 50)?.value).toEqual('Default Value')
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
