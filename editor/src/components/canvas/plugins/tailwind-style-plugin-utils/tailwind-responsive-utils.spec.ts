import type { Config } from 'tailwindcss/types/config'
import { getModifiers, screensConfigToScreenSizes } from './tailwind-responsive-utils'

describe('getModifiers', () => {
  it('returns empty array for non-media variants', () => {
    const variants = [{ type: 'hover', value: 'hover' }]
    const config: Config = { theme: { screens: {} } } as Config

    const result = getModifiers(variants, config)
    expect(result).toEqual([])
  })

  it('handles default screen sizes correctly', () => {
    const variants = [{ type: 'media', value: 'md' }]
    const config = null // null config should use defaults

    const result = getModifiers(variants, config)
    expect(result).toEqual([
      {
        type: 'media-size',
        size: {
          min: { value: 768, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'md' },
      },
    ])
  })

  it('handles custom screen sizes from config', () => {
    const variants = [{ type: 'media', value: 'custom' }]
    const config = {
      theme: {
        screens: {
          custom: '1000px',
        },
      },
    } as unknown as Config

    const result = getModifiers(variants, config)
    expect(result).toEqual([
      {
        type: 'media-size',
        size: {
          min: { value: 1000, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'custom' },
      },
    ])
  })

  it('handles min-max range screen sizes', () => {
    const variants = [{ type: 'media', value: 'tablet' }]
    const config = {
      theme: {
        screens: {
          tablet: { min: '768px', max: '1024px' },
        },
      },
    } as unknown as Config

    const result = getModifiers(variants, config)
    expect(result).toEqual([
      {
        type: 'media-size',
        size: {
          min: { value: 768, unit: 'px' },
          max: { value: 1024, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'tablet' },
      },
    ])
  })

  it('handles extended screen sizes', () => {
    const variants = [{ type: 'media', value: 'extra' }]
    const config = {
      theme: {
        screens: {
          sm: '640px',
        },
        extend: {
          screens: {
            extra: '1400px',
          },
        },
      },
    } as unknown as Config

    const result = getModifiers(variants, config)
    expect(result).toEqual([
      {
        type: 'media-size',
        size: {
          min: { value: 1400, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'extra' },
      },
    ])
  })

  it('handles multiple media variants', () => {
    const variants = [
      { type: 'media', value: 'sm' },
      { type: 'media', value: 'lg' },
    ]
    const config = null // use defaults

    const result = getModifiers(variants, config)
    expect(result).toEqual([
      {
        type: 'media-size',
        size: {
          min: { value: 640, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'sm' },
      },
      {
        type: 'media-size',
        size: {
          min: { value: 1024, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'lg' },
      },
    ])
  })

  it('filters out invalid screen sizes', () => {
    const variants = [
      { type: 'media', value: 'invalid' },
      { type: 'media', value: 'md' },
    ]
    const config = null // use defaults

    const result = getModifiers(variants, config)
    expect(result).toEqual([
      {
        type: 'media-size',
        size: {
          min: { value: 768, unit: 'px' },
        },
        modifierOrigin: { type: 'tailwind', variant: 'md' },
      },
    ])
  })
})

describe('screensConfigToScreenSizes', () => {
  it('returns default screen sizes when config is null', () => {
    const result = screensConfigToScreenSizes(null)
    expect(result).toEqual({
      sm: { min: { value: 640, unit: 'px' } },
      md: { min: { value: 768, unit: 'px' } },
      lg: { min: { value: 1024, unit: 'px' } },
      xl: { min: { value: 1280, unit: 'px' } },
      '2xl': { min: { value: 1536, unit: 'px' } },
    })
  })

  it('handles custom screen sizes', () => {
    const config = {
      theme: {
        screens: {
          mobile: '400px',
          tablet: '800px',
        },
      },
    } as unknown as Config

    const result = screensConfigToScreenSizes(config)
    expect(result).toEqual({
      mobile: { min: { value: 400, unit: 'px' } },
      tablet: { min: { value: 800, unit: 'px' } },
    })
  })

  it('handles min-max range screen sizes', () => {
    const config = {
      theme: {
        screens: {
          tablet: { min: '768px', max: '1024px' },
        },
      },
    } as unknown as Config

    const result = screensConfigToScreenSizes(config)
    expect(result).toEqual({
      tablet: {
        min: { value: 768, unit: 'px' },
        max: { value: 1024, unit: 'px' },
      },
    })
  })

  it('merges extended screen sizes with base config', () => {
    const config = {
      theme: {
        screens: {
          sm: '640px',
        },
        extend: {
          screens: {
            custom: '1400px',
          },
        },
      },
    } as unknown as Config

    const result = screensConfigToScreenSizes(config)
    expect(result).toEqual({
      sm: { min: { value: 640, unit: 'px' } },
      custom: { min: { value: 1400, unit: 'px' } },
    })
  })

  it('handles empty config objects', () => {
    const config = {
      theme: {},
    } as unknown as Config

    const result = screensConfigToScreenSizes(config)
    expect(result).toEqual({
      sm: { min: { value: 640, unit: 'px' } },
      md: { min: { value: 768, unit: 'px' } },
      lg: { min: { value: 1024, unit: 'px' } },
      xl: { min: { value: 1280, unit: 'px' } },
      '2xl': { min: { value: 1536, unit: 'px' } },
    })
  })
})
