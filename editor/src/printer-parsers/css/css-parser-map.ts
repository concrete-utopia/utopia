import { parseBackgroundSize, parseBGSize } from './css-parser-background-size'
import { parseLengthPercentage, parseLength, parsePercentage } from './css-parser-utils'
import { parseBorder } from './css-parser-border'

export const syntaxParsers = {
  "<'background-size'>": parseBackgroundSize,
  '<bg-size>': parseBGSize,
  '<length-percentage>': parseLengthPercentage,
  '<length>': parseLength,
  '<percentage>': parsePercentage,
  "<'border'>": parseBorder,
}
