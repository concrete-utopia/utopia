import type { Identifier, Dimension } from 'css-tree'
import type { CSSNumber } from '../inspector/common/css-utils'
// @media (min-width: 100px) and (max-width: 200em) => { min: { value: 100, unit: 'px' }, max: { value: 200, unit: 'em' } }
export type ScreenSize = {
  min?: CSSNumber
  max?: CSSNumber
}

export interface MediaQuery {
  type: 'MediaQuery'
  loc: null
  modifier: null
  mediaType: null
  condition?: {
    type: 'Condition'
    loc: null
    kind: 'media'
    children: Array<FeatureRange | Feature | Identifier>
  }
}

export interface FeatureRange {
  type: 'FeatureRange'
  loc: null
  kind: 'media'
  left?: Dimension | Identifier
  leftComparison: '<' | '>'
  middle: Dimension | Identifier
  rightComparison: '<' | '>'
  right?: Dimension | Identifier
}

export interface Feature {
  type: 'Feature'
  loc: null
  kind: 'media'
  name: 'min-width' | 'max-width'
  value?: Dimension
}
