import { NodeData } from 'react-vtree/dist/es/Tree'
import { GOOGLE_WEB_FONTS_KEY } from '../../../common/env-vars'
import { Either, left, right, traverseEither } from '../../../core/shared/either'
import { DescriptionParseError, descriptionParseError } from '../../../utils/value-parser-utils'
import {
  PushNewFontFamilyVariant,
  RemoveFontFamilyVariant,
} from './google-fonts-resources-list-search'
import { CSSFontWeight, CSSFontStyle } from '../../inspector/common/css-utils'

export const GoogleWebFontsURL = `https://www.googleapis.com/webfonts/v1/webfonts?key=${GOOGLE_WEB_FONTS_KEY}`

export type WebFontWeight = 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900

function cssFontWeightToWebFontWeight(value: CSSFontWeight): Either<string, WebFontWeight> {
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

function cssFontStyleToWebFontStyle(value: CSSFontStyle): Either<string, WebFontStyle> {
  switch (value) {
    case 'normal':
    case 'italic': {
      return right(value)
    }
    case 'oblique': {
      return right('normal')
    }
    default: {
      return left('Variable width webfonts from Google are not supported yet.')
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

export type GoogleFontVariantIdentifier =
  | '100'
  | '200'
  | '300'
  | 'regular'
  | '500'
  | '600'
  | '700'
  | '800'
  | '900'
  | '100italic'
  | '200italic'
  | '300italic'
  | 'italic'
  | '500italic'
  | '600italic'
  | '700italic'
  | '800italic'
  | '900italic'

const fontVariantMap: { [key in GoogleFontVariantIdentifier]: WebFontVariant } = {
  '100': webFontVariant(100, 'normal'),
  '200': webFontVariant(200, 'normal'),
  '300': webFontVariant(300, 'normal'),
  regular: webFontVariant(400, 'normal'),
  '500': webFontVariant(500, 'normal'),
  '600': webFontVariant(600, 'normal'),
  '700': webFontVariant(700, 'normal'),
  '800': webFontVariant(800, 'normal'),
  '900': webFontVariant(900, 'normal'),
  '100italic': webFontVariant(100, 'italic'),
  '200italic': webFontVariant(200, 'italic'),
  '300italic': webFontVariant(300, 'italic'),
  italic: webFontVariant(400, 'italic'),
  '500italic': webFontVariant(500, 'italic'),
  '600italic': webFontVariant(600, 'italic'),
  '700italic': webFontVariant(700, 'italic'),
  '800italic': webFontVariant(800, 'italic'),
  '900italic': webFontVariant(900, 'italic'),
}

function googleVariantToFontVariant(
  googleVariant: string,
): Either<DescriptionParseError, WebFontVariant> {
  if (fontVariantMap.hasOwnProperty(googleVariant)) {
    return right(fontVariantMap[googleVariant as GoogleFontVariantIdentifier])
  } else {
    return left(
      descriptionParseError('Only numeric font-weight keyword values are currently supported.'),
    )
  }
}

export function googleVariantStringsIntoWebFontVariants(
  variants: Array<string>,
): Either<DescriptionParseError, Array<WebFontVariant>> {
  const sorted = [...variants].sort()
  return traverseEither(googleVariantToFontVariant, sorted)
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
