import type { CSSFontFamily, CSSFontWeightAndStyle } from '../../../common/css-utils'

export type UtopiaFontWeight =
  | '100'
  | '200'
  | '300'
  | '400'
  | '500'
  | '600'
  | '700'
  | '800'
  | '900'
  | '100i'
  | '200i'
  | '300i'
  | '400i'
  | '500i'
  | '600i'
  | '700i'
  | '800i'
  | '900i'

export type FontStyle = 'normal' | 'italic' | 'oblique'

export interface TypefaceInfo {
  fontFamilyName: string
  cssFontFamily: CSSFontFamily
  fontWeightsAndStyles: Array<CSSFontWeightAndStyle>
}

export const LocalTypefaces: Array<TypefaceInfo> = [
  {
    fontFamilyName: 'San Francisco',
    cssFontFamily: ['-apple-system', 'BlinkMacSystemFont', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 100, fontStyle: 'normal' },
      { fontWeight: 100, fontStyle: 'italic' },
      { fontWeight: 200, fontStyle: 'normal' },
      { fontWeight: 200, fontStyle: 'italic' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'italic' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 500, fontStyle: 'normal' },
      { fontWeight: 500, fontStyle: 'italic' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 800, fontStyle: 'normal' },
      { fontWeight: 800, fontStyle: 'italic' },
      { fontWeight: 900, fontStyle: 'normal' },
      { fontWeight: 900, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Times New Roman',
    cssFontFamily: ['Times New Roman', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Georgia',
    cssFontFamily: ['Georgia', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'italic' },
    ],
  },
]

export const GoogleWebFontsTypefaces: Array<TypefaceInfo> = [
  {
    fontFamilyName: 'Arvo',
    cssFontFamily: ['Arvo', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'EB Garamond',
    cssFontFamily: ['EB Garamond', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 500, fontStyle: 'normal' },
      { fontWeight: 500, fontStyle: 'italic' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 800, fontStyle: 'normal' },
      { fontWeight: 800, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Lora',
    cssFontFamily: ['Lora', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Noto Sans',
    cssFontFamily: [
      'Noto Sans',
      'Noto Sans SC',
      'Noto Sans JP',
      'Noto Sans KR',
      'Noto Sans TC',
      'sans-serif',
    ],
    fontWeightsAndStyles: [
      { fontWeight: 100, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 500, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 900, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'Noto Serif',
    cssFontFamily: [
      'Noto Serif',
      'Noto Serif SC',
      'Noto Serif JP',
      'Noto Serif KR',
      'Noto Serif TC',
      'serif',
    ],
    fontWeightsAndStyles: [
      { fontWeight: 100, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 500, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 900, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'Open Sans',
    cssFontFamily: ['Open Sans', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'italic' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 800, fontStyle: 'normal' },
      { fontWeight: 800, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'PT Sans',
    cssFontFamily: ['PT Sans', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'PT Sans Narrow',
    cssFontFamily: ['PT Sans Narrow', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'PT Serif',
    cssFontFamily: ['PT Serif', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Playfair Display',
    cssFontFamily: ['Playfair Display', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 900, fontStyle: 'normal' },
      { fontWeight: 900, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Roboto',
    cssFontFamily: ['Roboto', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 100, fontStyle: 'normal' },
      { fontWeight: 100, fontStyle: 'italic' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'italic' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 500, fontStyle: 'normal' },
      { fontWeight: 500, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 900, fontStyle: 'normal' },
      { fontWeight: 900, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Roboto Condensed',
    cssFontFamily: ['Roboto Condensed', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'italic' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Roboto Mono',
    cssFontFamily: ['Roboto Mono', 'monospace'],
    fontWeightsAndStyles: [
      { fontWeight: 100, fontStyle: 'normal' },
      { fontWeight: 100, fontStyle: 'italic' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'italic' },
      { fontWeight: 400, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'Roboto Slab',
    cssFontFamily: ['Roboto Slab', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 100, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'Sorts Mill Goudy',
    cssFontFamily: ['Sorts Mill Goudy', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Source Code Pro',
    cssFontFamily: ['Source Code Pro', 'monospace'],
    fontWeightsAndStyles: [
      { fontWeight: 200, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 500, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 900, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'Source Sans Pro',
    cssFontFamily: ['Source Sans Pro', 'sans-serif'],
    fontWeightsAndStyles: [
      { fontWeight: 200, fontStyle: 'normal' },
      { fontWeight: 200, fontStyle: 'italic' },
      { fontWeight: 300, fontStyle: 'normal' },
      { fontWeight: 300, fontStyle: 'italic' },
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 400, fontStyle: 'italic' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'italic' },
      { fontWeight: 700, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'italic' },
      { fontWeight: 900, fontStyle: 'normal' },
      { fontWeight: 900, fontStyle: 'italic' },
    ],
  },
  {
    fontFamilyName: 'Source Serif Pro',
    cssFontFamily: ['Source Serif Pro', 'serif'],
    fontWeightsAndStyles: [
      { fontWeight: 400, fontStyle: 'normal' },
      { fontWeight: 600, fontStyle: 'normal' },
      { fontWeight: 700, fontStyle: 'normal' },
    ],
  },
  {
    fontFamilyName: 'ZCOOL QingKe HuangYou',
    cssFontFamily: ['ZCOOL QingKe HuangYou', 'sans-serif'],
    fontWeightsAndStyles: [{ fontWeight: 400, fontStyle: 'normal' }],
  },
]

export const defaultFontWeightsAndStyles: Array<CSSFontWeightAndStyle> = [
  { fontWeight: 100, fontStyle: 'normal' },
  { fontWeight: 100, fontStyle: 'italic' },
  { fontWeight: 200, fontStyle: 'normal' },
  { fontWeight: 200, fontStyle: 'italic' },
  { fontWeight: 300, fontStyle: 'normal' },
  { fontWeight: 300, fontStyle: 'italic' },
  { fontWeight: 400, fontStyle: 'normal' },
  { fontWeight: 400, fontStyle: 'italic' },
  { fontWeight: 500, fontStyle: 'normal' },
  { fontWeight: 500, fontStyle: 'italic' },
  { fontWeight: 600, fontStyle: 'normal' },
  { fontWeight: 600, fontStyle: 'italic' },
  { fontWeight: 700, fontStyle: 'normal' },
  { fontWeight: 700, fontStyle: 'italic' },
  { fontWeight: 800, fontStyle: 'normal' },
  { fontWeight: 800, fontStyle: 'italic' },
  { fontWeight: 900, fontStyle: 'normal' },
  { fontWeight: 900, fontStyle: 'italic' },
]

export type FontWeightInfo = {
  fontWeightAndStyleName: string
  style: Pick<React.CSSProperties, 'fontStyle' | 'fontWeight'>
}

export const fontWeightsList: { [key in UtopiaFontWeight]: FontWeightInfo } = {
  100: {
    fontWeightAndStyleName: 'Thin',
    style: { fontStyle: 'normal', fontWeight: 100 },
  },
  '100i': {
    fontWeightAndStyleName: 'Thin Italic',
    style: { fontStyle: 'italic', fontWeight: 100 },
  },
  200: {
    fontWeightAndStyleName: 'Extra Light',
    style: { fontStyle: 'normal', fontWeight: 200 },
  },
  '200i': {
    fontWeightAndStyleName: 'Extra Light Italic',
    style: { fontStyle: 'italic', fontWeight: 200 },
  },
  300: {
    fontWeightAndStyleName: 'Light',
    style: { fontStyle: 'normal', fontWeight: 300 },
  },
  '300i': {
    fontWeightAndStyleName: 'Light Italic',
    style: { fontStyle: 'italic', fontWeight: 300 },
  },
  400: {
    fontWeightAndStyleName: 'Regular',
    style: { fontStyle: 'normal', fontWeight: 400 },
  },
  '400i': {
    fontWeightAndStyleName: 'Regular Italic',
    style: { fontStyle: 'italic', fontWeight: 400 },
  },
  500: {
    fontWeightAndStyleName: 'Medium',
    style: { fontStyle: 'normal', fontWeight: 500 },
  },
  '500i': {
    fontWeightAndStyleName: 'Medium Italic',
    style: { fontStyle: 'italic', fontWeight: 500 },
  },
  600: {
    fontWeightAndStyleName: 'Semibold',
    style: { fontStyle: 'normal', fontWeight: 600 },
  },
  '600i': {
    fontWeightAndStyleName: 'Semibold Italic',
    style: { fontStyle: 'italic', fontWeight: 600 },
  },
  700: {
    fontWeightAndStyleName: 'Bold',
    style: { fontStyle: 'normal', fontWeight: 700 },
  },
  '700i': {
    fontWeightAndStyleName: 'Bold Italic',
    style: { fontStyle: 'italic', fontWeight: 700 },
  },
  800: {
    fontWeightAndStyleName: 'Extra Bold',
    style: { fontStyle: 'normal', fontWeight: 800 },
  },
  '800i': {
    fontWeightAndStyleName: 'Extra Bold Italic',
    style: { fontStyle: 'italic', fontWeight: 800 },
  },
  900: {
    fontWeightAndStyleName: 'Black',
    style: { fontStyle: 'normal', fontWeight: 900 },
  },
  '900i': {
    fontWeightAndStyleName: 'Black Italic',
    style: { fontStyle: 'italic', fontWeight: 900 },
  },
}

export const AllTypefaces = GoogleWebFontsTypefaces.concat(LocalTypefaces)

export const AllTypefacesAlphabeticallySorted = AllTypefaces.sort((a, b) => {
  const textA = a.fontFamilyName.toUpperCase()
  const textB = b.fontFamilyName.toUpperCase()
  return textA < textB ? -1 : textA > textB ? 1 : 0
})

export function cssFontWeightAndStyleToUtopiaFontWeight(
  fontWeightAndStyle: CSSFontWeightAndStyle,
): UtopiaFontWeight {
  return `${fontWeightAndStyle.fontWeight}${
    fontWeightAndStyle.fontStyle === 'italic' ? 'i' : ''
  }` as UtopiaFontWeight
}

export function fontFamilyArrayToCSSFontFamilyString(fontFamily: Array<string>): string {
  return fontFamily.join(', ')
}
