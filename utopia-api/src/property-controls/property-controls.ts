import type { CSSProperties } from 'react'
import type { PreferredContents } from '../core'

interface GenericControlProps<T> {
  label?: string
  folder?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: T
}

// these fields are shared among all RegularControlDescription. the helper function getControlSharedFields makes sure the types line up
// Ensure that the fields are also added to the object within `getControlSharedFields` for that typechecking.
interface ControlBaseFields extends GenericControlProps<unknown> {
  control: RegularControlType
}

// Base Level Controls

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

export interface CheckboxControlDescription extends GenericControlProps<unknown> {
  control: 'checkbox'
  disabledTitle?: string
  enabledTitle?: string
}

export interface ColorControlDescription extends GenericControlProps<unknown> {
  control: 'color'
}

export type AllowedEnumType = string | boolean | number | undefined | null
export interface BasicControlOption<T> {
  value: T
  label: string
}

export type BasicControlOptions<T> = AllowedEnumType[] | BasicControlOption<T>[]

export interface PopUpListControlDescription
  extends GenericControlProps<AllowedEnumType | BasicControlOption<unknown>> {
  control: 'popuplist'
  options: BasicControlOptions<unknown>
}

export interface ImportType {
  source: string // importSource
  name: string | null
  type: 'star' | 'default' | null
}

export interface ExpressionControlOption<T> {
  value: T
  expression: string
  label?: string
  requiredImport?: ImportType
}

export interface ExpressionPopUpListControlDescription extends GenericControlProps<unknown> {
  control: 'expression-popuplist'
  options: ExpressionControlOption<unknown>[]
}

export interface EulerControlDescription
  extends GenericControlProps<[number, number, number, string]> {
  control: 'euler'
}

export interface NoneControlDescription extends GenericControlProps<unknown> {
  control: 'none'
}

// prettier-ignore
export type Matrix3 = [
  number, number, number,
  number, number, number,
  number, number, number,
]

export interface Matrix3ControlDescription extends GenericControlProps<Matrix3> {
  control: 'matrix3'
}

// prettier-ignore
export type Matrix4 = [
  number, number, number, number,
  number, number, number, number,
  number, number, number, number,
  number, number, number, number,
]

export interface Matrix4ControlDescription extends GenericControlProps<Matrix4> {
  control: 'matrix4'
}

export interface NumberInputControlDescription extends GenericControlProps<unknown> {
  control: 'number-input'
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export interface RadioControlDescription
  extends GenericControlProps<AllowedEnumType | BasicControlOption<unknown>> {
  control: 'radio'
  options: BasicControlOptions<unknown>
}

export interface ExpressionInputControlDescription extends GenericControlProps<unknown> {
  control: 'expression-input'
}

export interface StringInputControlDescription extends GenericControlProps<unknown> {
  control: 'string-input'
  placeholder?: string
  obscured?: boolean
}

export interface HtmlInputControlDescription extends GenericControlProps<unknown> {
  control: 'html-input'
  placeholder?: string
  obscured?: boolean
}

export interface StyleControlsControlDescription extends GenericControlProps<unknown> {
  control: 'style-controls'
  placeholder?: CSSProperties
}

export type Vector2 = [number, number]

export interface Vector2ControlDescription extends GenericControlProps<Vector2> {
  control: 'vector2'
}

export type Vector3 = [number, number, number]

export interface Vector3ControlDescription extends GenericControlProps<Vector3> {
  control: 'vector3'
}

export type Vector4 = [number, number, number, number]

export interface Vector4ControlDescription extends GenericControlProps<Vector4> {
  control: 'vector4'
}

export interface JSXControlDescription extends GenericControlProps<unknown> {
  control: 'jsx'
  preferredContents?: PreferredContents | PreferredContents[]
}

export type BaseControlDescription =
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

export type PropertyControlsOptions<T> = Omit<ControlBaseFields, 'control'> & { defaultValue?: T }

// Higher Level Controls

export type HigherLevelControlType = 'array' | 'tuple' | 'object' | 'union'
export type RegularControlType = BaseControlType | HigherLevelControlType
export type ControlType = RegularControlType | 'folder'

export interface ArrayControlDescription extends GenericControlProps<unknown> {
  control: 'array'
  propertyControl: RegularControlDescription
  maxCount?: number
}

export interface ObjectControlDescription extends GenericControlProps<unknown> {
  control: 'object'
  object: { [prop: string]: RegularControlDescription }
}

export interface UnionControlDescription extends GenericControlProps<unknown> {
  control: 'union'
  controls: Array<RegularControlDescription>
}
export interface TupleControlDescription extends GenericControlProps<unknown> {
  control: 'tuple'
  propertyControls: RegularControlDescription[]
}

export type HigherLevelControlDescription =
  | ArrayControlDescription
  | ObjectControlDescription
  | TupleControlDescription
  | UnionControlDescription

export type RegularControlDescription = BaseControlDescription | HigherLevelControlDescription

// Please ensure that `property-controls-utils.ts` is kept up to date
// with any changes to this or the component types.
export type ControlDescription = RegularControlDescription

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

export type PropertyControls = {
  [key: string]: ControlDescription
}

export function addPropertyControls(component: unknown, propertyControls: PropertyControls): void {
  if (component != null) {
    ;(component as any)['propertyControls'] = propertyControls
  }
}
