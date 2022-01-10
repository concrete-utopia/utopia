import { ParseResult } from '../../utils/value-parser-utils'
import {
  ParsedPropertyControls,
  parsePropertyControls,
} from '../property-controls/property-controls-parser'

export const HtmlElementStyleObjectProps: ParseResult<ParsedPropertyControls> = parsePropertyControls(
  {
    style: {
      control: 'style-controls',
    },
  },
)
