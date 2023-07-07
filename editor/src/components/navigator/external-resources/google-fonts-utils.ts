import type { NodeData } from 'react-vtree/dist/es/Tree'
import { GOOGLE_WEB_FONTS_KEY } from '../../../common/env-vars'
import type { Either } from '../../../core/shared/either'
import { left, right } from '../../../core/shared/either'
import type { CSSFontStyle, CSSFontWeight } from '../../inspector/common/css-utils'
import type {
  PushNewFontFamilyVariant,
  RemoveFontFamilyVariant,
} from './google-fonts-resources-list-search'

export const GoogleWebFontsURL = `https://www.googleapis.com/webfonts/v1/webfonts?key=${GOOGLE_WEB_FONTS_KEY}`

export type WebFontWeight = 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900

export function cssFontWeightToWebFontWeight(value: CSSFontWeight): Either<string, WebFontWeight> {
  switch (value) {
    case 'normal': {
      return right(400)
    }
    case 'bold': {
      return right(600)
    }
    case 100:
    case 200:
    case 300:
    case 400:
    case 500:
    case 600:
    case 700:
    case 800:
    case 900: {
      return right(value)
    }
    default: {
      return left('Variable width webfonts from Google are not supported yet.')
    }
  }
}

export function cssFontStyleToWebFontStyle(value: CSSFontStyle): Either<string, WebFontStyle> {
  switch (value) {
    case 'normal':
    case 'italic': {
      return right(value)
    }
    case 'oblique': {
      return right('normal')
    }
    default: {
      return left('Variable italic webfonts from Google are not supported yet.')
    }
  }
}

type WebFontStyle = 'normal' | 'italic'
export function isFontVariantWeight(value: number): value is WebFontWeight {
  return weightAxisPrettyNames.hasOwnProperty(value)
}
const weightAxisPrettyNames: { [key in WebFontWeight]: string } = {
  100: 'Thin',
  200: 'Extra-light',
  300: 'Light',
  400: 'Regular',
  500: 'Medium',
  600: 'Semi-bold',
  700: 'Bold',
  800: 'Extra-bold',
  900: 'Black',
}

export const defaultWebFontWeightsAndStyles: Array<WebFontVariant> = [
  webFontVariant(100, 'normal'),
  webFontVariant(100, 'italic'),
  webFontVariant(200, 'normal'),
  webFontVariant(200, 'italic'),
  webFontVariant(300, 'normal'),
  webFontVariant(300, 'italic'),
  webFontVariant(400, 'normal'),
  webFontVariant(400, 'italic'),
  webFontVariant(500, 'normal'),
  webFontVariant(500, 'italic'),
  webFontVariant(600, 'normal'),
  webFontVariant(600, 'italic'),
  webFontVariant(700, 'normal'),
  webFontVariant(700, 'italic'),
  webFontVariant(800, 'normal'),
  webFontVariant(800, 'italic'),
  webFontVariant(900, 'normal'),
  webFontVariant(900, 'italic'),
]

export interface WebFontFamily {
  type: 'web-font-family'
  familyName: string
  variants: Array<WebFontVariant>
}

export function webFontFamily(familyName: string, variants: Array<WebFontVariant>): WebFontFamily {
  return {
    type: 'web-font-family',
    familyName,
    variants,
  }
}

export interface WebFontFamilyVariant {
  type: 'web-font-family-variant'
  familyName: string
  fontVariant: WebFontVariant
}

export function webFontFamilyVariant(
  familyName: string,
  fontVariant: WebFontVariant,
): WebFontFamilyVariant {
  return {
    type: 'web-font-family-variant',
    familyName,
    fontVariant,
  }
}

export interface WebFontVariant {
  type: 'web-font-variant'
  webFontWeight: WebFontWeight
  webFontStyle: WebFontStyle
}

export function webFontVariant(
  webFontWeight: WebFontWeight,
  webFontStyle: WebFontStyle,
): WebFontVariant {
  return {
    type: 'web-font-variant',
    webFontWeight,
    webFontStyle,
  }
}

export function prettyNameForFontVariant(value: WebFontVariant): string {
  const prettyWeightName = weightAxisPrettyNames[value.webFontWeight]
  const italicKeyword = value.webFontStyle === 'italic' ? ' italic' : ''
  return prettyWeightName + italicKeyword
}

export interface GoogleFontFamilyOption {
  label: string
  variants: Array<WebFontFamilyVariant>
}

export function googleFontFamilyOption(
  label: string,
  variants: Array<WebFontFamilyVariant>,
): GoogleFontFamilyOption {
  return {
    label,
    variants,
  }
}

export interface FontsRoot extends NodeData {
  type: 'root'
  children: Array<FontFamilyData>
}

export interface FontFamilyData extends NodeData {
  type: 'font-family'
  familyName: string
  children: Array<FontVariantData>
}

export function fontFamilyData(
  familyName: string,
  children: Array<FontVariantData>,
): FontFamilyData {
  return {
    type: 'font-family',
    id: familyName,
    familyName,
    children,
    isOpenByDefault: false,
  }
}

export interface FontVariantData extends NodeData {
  type: 'font-variant'
  variant: WebFontFamilyVariant
  isDownloaded: boolean
  pushNewFontFamilyVariant: PushNewFontFamilyVariant
  removeFontFamilyVariant: RemoveFontFamilyVariant
}
export function fontVariantData(
  variant: WebFontFamilyVariant,
  isDownloaded: boolean,
  pushNewFontFamilyVariant: PushNewFontFamilyVariant,
  removeFontFamilyVariant: RemoveFontFamilyVariant,
): FontVariantData {
  return {
    type: 'font-variant',
    id: fontVariantID(variant),
    variant,
    isOpenByDefault: false,
    isDownloaded,
    pushNewFontFamilyVariant,
    removeFontFamilyVariant,
  }
}

export function fontVariantID(variant: WebFontFamilyVariant): string {
  return `${variant.familyName} ${prettyNameForFontVariant(variant.fontVariant)}`
}

export type FontNode = FontsRoot | FontFamilyData | FontVariantData

export interface SystemDefaultTypeface {
  type: 'system-default-typeface'
  name: 'San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
}
export const systemDefaultTypeface: SystemDefaultTypeface = {
  type: 'system-default-typeface',
  name: 'San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
}

export interface GoogleFontsTypeface {
  type: 'google-fonts-typeface'
  name: string
  variants: Array<WebFontVariant>
}

export function googleFontsTypeface(
  name: string,
  variants: Array<WebFontVariant>,
): GoogleFontsTypeface {
  return {
    type: 'google-fonts-typeface',
    name,
    variants,
  }
}
