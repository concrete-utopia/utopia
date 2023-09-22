export interface UtopiColor {
  value: string
  cssValue: string
}

export function createUtopiColor(
  baseColor: string,
  path: string = '--utopitheme-not-set',
): UtopiColor {
  return { value: `var(${path})`, cssValue: baseColor }
}

export function enforceUtopiColorTheme<T extends { [K in keyof T]: UtopiColor }>(theme: T): T {
  return theme
}
