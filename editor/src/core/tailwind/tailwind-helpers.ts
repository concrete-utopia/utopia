import type { PropertyPath } from 'utopia-shared/src/types'
import * as PP from '../shared/property-path'

const tailwindDimensionMap: { [key: string]: string } = {
  // Width classes
  'w-0': '0px',
  'w-1': '0.25rem', // 4px
  'w-2': '0.5rem', // 8px
  'w-3': '0.75rem', // 12px
  'w-4': '1rem', // 16px
  'w-5': '1.25rem', // 20px
  'w-6': '1.5rem', // 24px
  'w-8': '2rem', // 32px
  'w-10': '2.5rem', // 40px
  'w-12': '3rem', // 48px
  'w-16': '4rem', // 64px
  'w-20': '5rem', // 80px
  'w-24': '6rem', // 96px
  'w-32': '8rem', // 128px
  'w-40': '10rem', // 160px
  'w-48': '12rem', // 192px
  'w-56': '14rem', // 224px
  'w-64': '16rem', // 256px
  'w-auto': 'auto',
  'w-full': '100%',
  'w-screen': '100vw',
  // Height classes
  'h-0': '0px',
  'h-1': '0.25rem', // 4px
  'h-2': '0.5rem', // 8px
  'h-3': '0.75rem', // 12px
  'h-4': '1rem', // 16px
  'h-5': '1.25rem', // 20px
  'h-6': '1.5rem', // 24px
  'h-8': '2rem', // 32px
  'h-10': '2.5rem', // 40px
  'h-12': '3rem', // 48px
  'h-16': '4rem', // 64px
  'h-20': '5rem', // 80px
  'h-24': '6rem', // 96px
  'h-32': '8rem', // 128px
  'h-40': '10rem', // 160px
  'h-48': '12rem', // 192px
  'h-56': '14rem', // 224px
  'h-64': '16rem', // 256px
  'h-auto': 'auto',
  'h-full': '100%',
  'h-screen': '100vh',
  // Add more mappings as needed
}

export type TailwindProp = 'width' | 'height'

export function getTailwindPropFromPropertyPath(property: PropertyPath): TailwindProp | null {
  const lastProp = PP.lastPartToString(property)
  switch (lastProp) {
    case 'width':
    case 'height':
      return lastProp
    default:
      return null
  }
}

export function convertTailwindDimensionToPixels(tailwindClass: string): string | null {
  return tailwindDimensionMap[tailwindClass] ?? null
}

export function convertPixelsToTailwindDimension(
  pixelValue: number,
  dimensionType: TailwindProp,
): string | null {
  // Convert rem values to pixels for comparison
  const remToPx = (rem: string): number => parseFloat(rem) * 16

  let closestClass: string | null = null
  let closestValue: number = Infinity

  for (const [className, value] of Object.entries(tailwindDimensionMap)) {
    if (
      (dimensionType === 'width' && !className.startsWith('w-')) ||
      (dimensionType === 'height' && !className.startsWith('h-'))
    ) {
      continue
    }

    let numericValue: number

    if (value.endsWith('rem')) {
      numericValue = remToPx(value)
    } else if (value.endsWith('px')) {
      numericValue = parseFloat(value)
    } else {
      continue // Skip non-pixel/rem values like 'auto', '100%', '100vw', '100vh'
    }

    const difference = Math.abs(numericValue - pixelValue)

    if (difference < closestValue) {
      closestValue = difference
      closestClass = className
    }
  }

  return closestClass
}
