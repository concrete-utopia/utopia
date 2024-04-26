import type React from 'react'
import type { PropertyControls } from '../property-controls/property-controls'

export type AdditionalImports = string | string[]

export interface ComponentInsertOption {
  imports?: AdditionalImports
  label: string
  code: string
}

export type ComponentExample =
  // for intrinsic elements
  | { name: string }

  // if the actual component is passed, we try to infer the component
  // name/module name from that
  | { component: any }

  // detailed description with the path to import the element from and component
  // variants
  | ComponentInsertOption

export type PreferredContents =
  | 'text'
  | {
      component: string
      moduleName?: string
      variants: ComponentExample | ComponentExample[]
    }

export interface ChildrenSpec {
  // specifies what component(s) are preferred.
  // `undefined`: any component can be rendered
  // `'text'`: means only JSX text is accepted
  // `RendersComponent`: detailed spec
  preferredContents?: PreferredContents | PreferredContents[]
}

export type Children = 'supported' | 'not-supported' | ChildrenSpec

export const FocusOptions = ['default', 'always', 'never'] as const
export type Focus = (typeof FocusOptions)[number]

export const StylingOptions = ['layout', 'layout-system', 'visual', 'typography'] as const
export type Styling = (typeof StylingOptions)[number]

export type InspectorSpec = 'all' | Styling[]

export const EmphasisOptions = ['subdued', 'regular', 'emphasized'] as const
export type Emphasis = (typeof EmphasisOptions)[number]

export const IconOptions = ['column', 'row', 'regular'] as const // and others
export type Icon = (typeof IconOptions)[number]

export interface ComponentToRegister {
  component: any
  properties: PropertyControls
  children?: Children
  focus?: Focus
  inspector?: InspectorSpec
  emphasis?: Emphasis
  icon?: Icon
  variants?: ComponentExample | Array<ComponentExample>
}

export type RawSingleBorderWidth = number | string
export type RawSplitBorderWidth = [
  RawSingleBorderWidth,
  RawSingleBorderWidth,
  RawSingleBorderWidth,
  RawSingleBorderWidth,
]
export type RawBorderWidth = RawSingleBorderWidth | RawSplitBorderWidth

export interface ShadowAndBorderParams {
  boxShadow?: string
  borderStyle?: 'solid' | 'none'
  borderWidth?: RawBorderWidth
  borderColor?: string
}

export const KrazyGeorgeTestUrl = 'testAsset/krazyGeorge.jpg'

function printEnabled(value: string, enabled: boolean): string {
  return `${enabled ? '' : '/*'}${value}${enabled ? '' : '*/'}`
}

function singleBorderWidthToString(borderWidth: RawSingleBorderWidth): string {
  if (typeof borderWidth === 'number') {
    return `${borderWidth}px`
  } else {
    return borderWidth
  }
}

function singleBorderToString(borderWidth: RawSingleBorderWidth, borderColor: string): string {
  return `inset 0 0 0 ${singleBorderWidthToString(borderWidth)} ${borderColor}`
}

function splitBorderToString(borderWidth: RawSplitBorderWidth, borderColor: string): string {
  return [
    `inset 0 ${singleBorderWidthToString(borderWidth[0])} 0 0 ${borderColor}`,
    `inset -${singleBorderWidthToString(borderWidth[1])} 0 0 0 ${borderColor}`,
    `inset 0 -${singleBorderWidthToString(borderWidth[2])} 0 0 ${borderColor}`,
    `inset ${singleBorderWidthToString(borderWidth[3])} 0 0 0 ${borderColor}`,
  ].join(',\n')
}

function borderToString(borderWidth: RawBorderWidth, borderColor: string): string | null {
  if (Array.isArray(borderWidth)) {
    return borderWidth.length === 4 ? splitBorderToString(borderWidth, borderColor) : null
  } else {
    return singleBorderToString(borderWidth, borderColor)
  }
}

export function cssBorderToBoxShadowString(
  borderStyle?: 'solid' | 'none',
  borderWidth?: RawBorderWidth,
  borderColor?: string,
): string {
  if (borderColor != null && borderWidth != null) {
    const enabled = borderStyle !== 'none'
    const borderString = borderToString(borderWidth, borderColor)
    return borderString == null ? '' : printEnabled(borderString, enabled)
  }
  return ''
}

export const UtopiaUtils = {
  shadowAndBorder(params: ShadowAndBorderParams): string {
    let allShadows: Array<string> = []
    if (params.borderStyle !== 'none') {
      const border = cssBorderToBoxShadowString(
        params.borderStyle,
        params.borderWidth,
        params.borderColor,
      )
      if (border.length > 0) {
        allShadows.push(border)
      }
    }
    if (params.boxShadow != null && typeof params.boxShadow === 'string') {
      allShadows.push(params.boxShadow)
    }
    return allShadows.join(', ')
  },
  disabled(p: any): undefined {
    return undefined
  },
}
