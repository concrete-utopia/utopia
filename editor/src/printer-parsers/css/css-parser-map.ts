import { parseBackgroundSize, parseBGSize } from './css-parser-background-size'
import { parseBorder } from './css-parser-border'
import { parseBorderColor } from './css-parser-border-color'
import { parseBorderSize } from './css-parser-border-size'
import { parseBorderStyle } from './css-parser-border-style'
import { parseFlex } from './css-parser-flex'
import { parsePadding } from './css-parser-padding'
import {
  parseAlphaValue,
  parseAngle,
  parseHSLColor,
  parseHue,
  parseLength,
  parseLengthPercentage,
  parseLexedColor,
  parseNumber,
  parsePercentage,
  parseRGBColor,
} from './css-parser-utils'

export const syntaxParsers = {
  '<alpha-value>': parseAlphaValue,
  '<angle>': parseAngle,
  "<'background-size'>": parseBackgroundSize,
  '<bg-size>': parseBGSize,
  "<'border'>": parseBorder,
  "<'border-size'>": parseBorderSize,
  "<'border-style'>": parseBorderStyle,
  "<'border-color'>": parseBorderColor,
  '<color>': parseLexedColor,
  '<hue>': parseHue,
  '<length-percentage>': parseLengthPercentage,
  '<length>': parseLength,
  '<number>': parseNumber,
  '<padding>': parsePadding,
  '<percentage>': parsePercentage,
  '<flex>': parseFlex,
  '<rgb()>': parseRGBColor,
  '<rgba()>': parseRGBColor,
  '<hsl()>': parseHSLColor,
  '<hsla()>': parseHSLColor,
}
