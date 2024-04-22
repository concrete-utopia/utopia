import type { CSSProperties } from 'react'
import type { ComponentInfo } from './code-file'

export type BaseControlType =
  | 'checkbox'
  | 'color'
  | 'euler'
  | 'expression-input'
  | 'expression-popuplist'
  | 'matrix3'
  | 'matrix4'
  | 'none'
  | 'number-input'
  | 'popuplist'
  | 'radio'
  | 'string-input'
  | 'html-input'
  | 'style-controls'
  | 'vector2'
  | 'vector3'
  | 'vector4'
  | 'jsx'

export interface CheckboxControlDescription {
  control: 'checkbox'
  label?: string
  visibleByDefault?: boolean
  disabledTitle?: string
  enabledTitle?: string
  required?: boolean
  defaultValue?: unknown
}

export interface ColorControlDescription {
  control: 'color'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

export type AllowedEnumType = string | boolean | number | undefined | null

export interface BasicControlOption<T> {
  value: T
  label: string
}

export type BasicControlOptions<T> = AllowedEnumType[] | BasicControlOption<T>[]

export interface PopUpListControlDescription {
  control: 'popuplist'
  label?: string
  visibleByDefault?: boolean
  options: BasicControlOptions<unknown>
  required?: boolean
  defaultValue?: AllowedEnumType | BasicControlOption<unknown>
}
export interface ImportType {
  source: string
  name: string | null
  type: 'star' | 'default' | null
}

export interface ExpressionControlOption<T> {
  value: T
  expression: string
  label?: string
  requiredImport?: ImportType
}

export interface ExpressionPopUpListControlDescription {
  control: 'expression-popuplist'
  label?: string
  visibleByDefault?: boolean
  options: ExpressionControlOption<unknown>[]
  required?: boolean
  defaultValue?: unknown
}

export interface EulerControlDescription {
  control: 'euler'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: [number, number, number, string]
}

export interface NoneControlDescription {
  control: 'none'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

export type Matrix3 = [number, number, number, number, number, number, number, number, number]

export interface Matrix3ControlDescription {
  control: 'matrix3'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Matrix3
}

export type Matrix4 = [
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
]

export interface Matrix4ControlDescription {
  control: 'matrix4'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Matrix4
}

export interface NumberInputControlDescription {
  control: 'number-input'
  label?: string
  visibleByDefault?: boolean
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface RadioControlDescription {
  control: 'radio'
  label?: string
  visibleByDefault?: boolean
  options: BasicControlOptions<unknown>
  required?: boolean
  defaultValue?: AllowedEnumType | BasicControlOption<unknown>
}

export interface ExpressionInputControlDescription {
  control: 'expression-input'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface StringInputControlDescription {
  control: 'string-input'
  label?: string
  visibleByDefault?: boolean
  placeholder?: string
  obscured?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface HtmlInputControlDescription {
  control: 'html-input'
  label?: string
  visibleByDefault?: boolean
  placeholder?: string
  obscured?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface StyleControlsControlDescription {
  control: 'style-controls'
  label?: string
  visibleByDefault?: boolean
  placeholder?: CSSProperties
  required?: boolean
  defaultValue?: unknown
}

export interface Vector2ControlDescription {
  control: 'vector2'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: [number, number]
}

export interface Vector3ControlDescription {
  control: 'vector3'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: [number, number, number]
}

export interface Vector4ControlDescription {
  control: 'vector4'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: [number, number, number, number]
}

export interface PreferredChildComponentDescriptor {
  name: string
  moduleName: string | null
  variants: Array<ComponentInfo>
}

export interface JSXControlDescription {
  control: 'jsx'
  label?: string
  visibleByDefault?: boolean
  preferredChildComponents: Array<PreferredChildComponentDescriptor>
  required?: boolean
  defaultValue?: unknown
}
export declare type BaseControlDescription =
  | CheckboxControlDescription
  | ColorControlDescription
  | ExpressionInputControlDescription
  | ExpressionPopUpListControlDescription
  | EulerControlDescription
  | NoneControlDescription
  | Matrix3ControlDescription
  | Matrix4ControlDescription
  | NumberInputControlDescription
  | RadioControlDescription
  | PopUpListControlDescription
  | StringInputControlDescription
  | HtmlInputControlDescription
  | StyleControlsControlDescription
  | Vector2ControlDescription
  | Vector3ControlDescription
  | Vector4ControlDescription
  | JSXControlDescription

export type HigherLevelControlType = 'array' | 'tuple' | 'object' | 'union'

export type RegularControlType = BaseControlType | HigherLevelControlType

export type ControlType = RegularControlType | 'folder'

export interface ArrayControlDescription {
  control: 'array'
  label?: string
  visibleByDefault?: boolean
  propertyControl: RegularControlDescription
  maxCount?: number
  required?: boolean
  defaultValue?: unknown
}

export interface ObjectControlDescription {
  control: 'object'
  label?: string
  visibleByDefault?: boolean
  object: {
    [prop: string]: RegularControlDescription
  }
  required?: boolean
  defaultValue?: unknown
}

export interface UnionControlDescription {
  control: 'union'
  label?: string
  visibleByDefault?: boolean
  controls: Array<RegularControlDescription>
  required?: boolean
  defaultValue?: unknown
}

export interface TupleControlDescription {
  control: 'tuple'
  label?: string
  visibleByDefault?: boolean
  propertyControls: RegularControlDescription[]
  required?: boolean
  defaultValue?: unknown
}

export interface FolderControlDescription {
  control: 'folder'
  label?: string
  controls: PropertyControls
}

export type HigherLevelControlDescription =
  | ArrayControlDescription
  | ObjectControlDescription
  | TupleControlDescription
  | UnionControlDescription

export type RegularControlDescription = BaseControlDescription | HigherLevelControlDescription
export type ControlDescription = RegularControlDescription | FolderControlDescription

export type PropertyControls = {
  [key: string]: ControlDescription
}

export function isBaseControlDescription(
  control: ControlDescription,
): control is BaseControlDescription {
  switch (control.control) {
    case 'checkbox':
    case 'color':
    case 'euler':
    case 'expression-input':
    case 'expression-popuplist':
    case 'matrix3':
    case 'matrix4':
    case 'none':
    case 'number-input':
    case 'popuplist':
    case 'radio':
    case 'string-input':
    case 'html-input':
    case 'style-controls':
    case 'vector2':
    case 'vector3':
    case 'vector4':
    case 'jsx':
      return true
    case 'array':
    case 'object':
    case 'tuple':
    case 'union':
    case 'folder':
      return false
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}

export function isHigherLevelControlDescription(
  control: ControlDescription,
): control is HigherLevelControlDescription {
  return !isBaseControlDescription(control)
}
