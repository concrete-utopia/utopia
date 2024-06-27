import type { PropertyPath } from 'utopia-shared/src/types'
import * as PP from '../shared/property-path'
import type { MapLike } from 'typescript'
import { twindInstance } from './tailwind'
import { emptyComplexMap } from '../../utils/map'

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

  // Padding classes
  'p-0': '0px',
  'p-1': '0.25rem', // 4px
  'p-2': '0.5rem', // 8px
  'p-3': '0.75rem', // 12px
  'p-4': '1rem', // 16px
  'p-5': '1.25rem', // 20px
  'p-6': '1.5rem', // 24px
  'p-8': '2rem', // 32px
  'p-10': '2.5rem', // 40px
  'p-12': '3rem', // 48px
  'p-16': '4rem', // 64px
  'p-20': '5rem', // 80px
  'p-24': '6rem', // 96px
  'p-32': '8rem', // 128px
  'p-40': '10rem', // 160px
  'p-48': '12rem', // 192px
  'p-56': '14rem', // 224px
  'p-64': '16rem', // 256px

  // Padding left classes
  'pl-0': '0px',
  'pl-1': '0.25rem', // 4px
  'pl-2': '0.5rem', // 8px
  'pl-3': '0.75rem', // 12px
  'pl-4': '1rem', // 16px
  'pl-5': '1.25rem', // 20px
  'pl-6': '1.5rem', // 24px
  'pl-8': '2rem', // 32px
  'pl-10': '2.5rem', // 40px
  'pl-12': '3rem', // 48px
  'pl-16': '4rem', // 64px
  'pl-20': '5rem', // 80px
  'pl-24': '6rem', // 96px
  'pl-32': '8rem', // 128px
  'pl-40': '10rem', // 160px
  'pl-48': '12rem', // 192px
  'pl-56': '14rem', // 224px
  'pl-64': '16rem', // 256px

  // Padding right classes
  'pr-0': '0px',
  'pr-1': '0.25rem', // 4px
  'pr-2': '0.5rem', // 8px
  'pr-3': '0.75rem', // 12px
  'pr-4': '1rem', // 16px
  'pr-5': '1.25rem', // 20px
  'pr-6': '1.5rem', // 24px
  'pr-8': '2rem', // 32px
  'pr-10': '2.5rem', // 40px
  'pr-12': '3rem', // 48px
  'pr-16': '4rem', // 64px
  'pr-20': '5rem', // 80px
  'pr-24': '6rem', // 96px
  'pr-32': '8rem', // 128px
  'pr-40': '10rem', // 160px
  'pr-48': '12rem', // 192px
  'pr-56': '14rem', // 224px
  'pr-64': '16rem', // 256px

  // Padding bottom classes
  'pb-0': '0px',
  'pb-1': '0.25rem', // 4px
  'pb-2': '0.5rem', // 8px
  'pb-3': '0.75rem', // 12px
  'pb-4': '1rem', // 16px
  'pb-5': '1.25rem', // 20px
  'pb-6': '1.5rem', // 24px
  'pb-8': '2rem', // 32px
  'pb-10': '2.5rem', // 40px
  'pb-12': '3rem', // 48px
  'pb-16': '4rem', // 64px
  'pb-20': '5rem', // 80px
  'pb-24': '6rem', // 96px
  'pb-32': '8rem', // 128px
  'pb-40': '10rem', // 160px
  'pb-48': '12rem', // 192px
  'pb-56': '14rem', // 224px
  'pb-64': '16rem', // 256px

  // Padding top classes
  'pt-0': '0px',
  'pt-1': '0.25rem', // 4px
  'pt-2': '0.5rem', // 8px
  'pt-3': '0.75rem', // 12px
  'pt-4': '1rem', // 16px
  'pt-5': '1.25rem', // 20px
  'pt-6': '1.5rem', // 24px
  'pt-8': '2rem', // 32px
  'pt-10': '2.5rem', // 40px
  'pt-12': '3rem', // 48px
  'pt-16': '4rem', // 64px
  'pt-20': '5rem', // 80px
  'pt-24': '6rem', // 96px
  'pt-32': '8rem', // 128px
  'pt-40': '10rem', // 160px
  'pt-48': '12rem', // 192px
  'pt-56': '14rem', // 224px
  'pt-64': '16rem', // 256px

  // Add more mappings as needed
}

export type TailwindProp =
  | 'width'
  | 'height'
  | 'padding-left'
  | 'padding-right'
  | 'padding-bottom'
  | 'padding-top'

export function getTailwindPropFromPropertyPath(property: PropertyPath): TailwindProp | null {
  const lastProp = PP.lastPartToString(property)
  switch (lastProp) {
    case 'width':
    case 'height':
    case 'padding-left':
    case 'padding-right':
    case 'padding-bottom':
    case 'padding-top':
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
      (dimensionType === 'height' && !className.startsWith('h-')) ||
      (dimensionType === 'padding-top' && !className.startsWith('pt-')) ||
      (dimensionType === 'padding-bottom' && !className.startsWith('pb-')) ||
      (dimensionType === 'padding-left' && !className.startsWith('pl-')) ||
      (dimensionType === 'padding-right' && !className.startsWith('pr-'))
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

export function getTailwindConfigurationForSection(tailwindSection: string): MapLike<string> {
  return (twindInstance.current?.instance.theme('padding') ?? {}) as MapLike<string>
}

export function getTailwindSnapPointsInPixelsForSection(tailwindSection: string): number[] {
  const padding = getTailwindConfigurationForSection(tailwindSection)
  return Object.values(padding).map((value: string) => {
    // if 'rem', or 'em', use the CSS OM to convert to px
    if (value.endsWith('rem') || value.endsWith('em')) {
      return parseFloat(value) * 16
    } else {
      return parseFloat(value)
    }
  })
}
