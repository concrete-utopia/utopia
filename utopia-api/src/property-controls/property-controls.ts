import type { CSSProperties } from 'react'
import { fastForEach } from '../utils'

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
  | 'style-controls'
  | 'vector2'
  | 'vector3'
  | 'vector4'

interface AbstractControlDescription<T extends ControlType> {
  label?: string
  control: T
  defaultValue?: unknown
  visibleByDefault?: boolean
}

interface AbstractBaseControlDescription<T extends BaseControlType>
  extends AbstractControlDescription<T> {}

export interface CheckboxControlDescription extends AbstractBaseControlDescription<'checkbox'> {
  defaultValue?: boolean
  disabledTitle?: string
  enabledTitle?: string
}

export interface ColorControlDescription extends AbstractBaseControlDescription<'color'> {
  defaultValue?: string
}

export type AllowedEnumType = string | boolean | number | undefined | null
export interface BasicControlOption<T> {
  value: T
  label: string
}

export type BasicControlOptions<T> = AllowedEnumType[] | BasicControlOption<T>[]

export interface PopUpListControlDescription extends AbstractBaseControlDescription<'popuplist'> {
  defaultValue?: unknown
  options: BasicControlOptions<unknown>
}

export interface ImportType {
  source: string // importSource
  name: string
  type: 'star' | 'default' | null
}

export interface ExpressionControlOption<T> {
  value: T
  expression: string
  label?: string
  requiredImport?: ImportType
}

export interface ExpressionPopUpListControlDescription
  extends AbstractBaseControlDescription<'expression-popuplist'> {
  defaultValue?: ExpressionControlOption<unknown>
  options: ExpressionControlOption<unknown>[]
}

export interface EulerControlDescription extends AbstractBaseControlDescription<'euler'> {
  defaultValue?: [number, number, number, string]
}

export interface NoneControlDescription extends AbstractBaseControlDescription<'none'> {
  defaultValue?: never
}

export interface Matrix3ControlDescription extends AbstractBaseControlDescription<'matrix3'> {
  defaultValue?: [number, number, number, number, number, number, number, number, number]
}

export interface Matrix4ControlDescription extends AbstractBaseControlDescription<'matrix4'> {
  defaultValue?: [
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
}

export interface NumberInputControlDescription
  extends AbstractBaseControlDescription<'number-input'> {
  defaultValue?: number | null
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export interface RadioControlDescription extends AbstractBaseControlDescription<'radio'> {
  defaultValue?: unknown
  options: BasicControlOptions<unknown>
}

export interface ExpressionInputControlDescription
  extends AbstractBaseControlDescription<'expression-input'> {
  defaultValue?: unknown
}

export interface StringInputControlDescription
  extends AbstractBaseControlDescription<'string-input'> {
  defaultValue?: string
  placeholder?: string
  obscured?: boolean
}

export interface StyleControlsControlDescription
  extends AbstractBaseControlDescription<'style-controls'> {
  defaultValue?: CSSProperties
  placeholder?: CSSProperties
}
export interface Vector2ControlDescription extends AbstractBaseControlDescription<'vector2'> {
  defaultValue?: [number, number]
}

export interface Vector3ControlDescription extends AbstractBaseControlDescription<'vector3'> {
  defaultValue?: [number, number, number]
}

export interface Vector4ControlDescription extends AbstractBaseControlDescription<'vector4'> {
  defaultValue?: [number, number, number, number]
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
  | StyleControlsControlDescription
  | Vector2ControlDescription
  | Vector3ControlDescription
  | Vector4ControlDescription

// Higher Level Controls

export type HigherLevelControlType = 'array' | 'object' | 'union'
export type RegularControlType = BaseControlType | HigherLevelControlType
export type ControlType = RegularControlType | 'folder'

interface AbstractHigherLevelControlDescription<T extends HigherLevelControlType>
  extends AbstractControlDescription<T> {}

export interface ArrayControlDescription extends AbstractHigherLevelControlDescription<'array'> {
  defaultValue?: unknown[]
  propertyControl: RegularControlDescription
  maxCount?: number
}

export interface ObjectControlDescription extends AbstractHigherLevelControlDescription<'object'> {
  defaultValue?: unknown
  object: { [prop: string]: RegularControlDescription }
}

export interface UnionControlDescription extends AbstractHigherLevelControlDescription<'union'> {
  defaultValue?: unknown
  controls: Array<RegularControlDescription>
}

export interface FolderControlDescription {
  control: 'folder'
  label?: string
  controls: PropertyControls
}

export type HigherLevelControlDescription =
  | ArrayControlDescription
  | ObjectControlDescription
  | UnionControlDescription

export type RegularControlDescription = BaseControlDescription | HigherLevelControlDescription

// Please ensure that `property-controls-utils.ts` is kept up to date
// with any changes to this or the component types.
export type ControlDescription = RegularControlDescription | FolderControlDescription

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
    case 'style-controls':
    case 'vector2':
    case 'vector3':
    case 'vector4':
      return true
    case 'array':
    case 'object':
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

export type PropertyControls = {
  [key: string]: ControlDescription
}

export function addPropertyControls(component: unknown, propertyControls: PropertyControls): void {
  if (component != null) {
    ;(component as any)['propertyControls'] = propertyControls
  }
}

export function getDefaultProps(propertyControls: PropertyControls): { [prop: string]: unknown } {
  let defaults: { [prop: string]: unknown } = {}
  const propKeys = Object.keys(propertyControls)
  fastForEach(propKeys, (prop) => {
    const control = propertyControls[prop]
    if (control != null) {
      if (isBaseControlDescription(control) || isHigherLevelControlDescription(control)) {
        if (control.defaultValue != null) {
          defaults[prop] = control.defaultValue
        }
      } else {
        defaults = {
          ...defaults,
          ...getDefaultProps(control.controls),
        }
      }
    }
  })
  return defaults
}

export function expression<T>(
  value: T,
  expressionString: string,
  requiredImport?: ImportType,
): ExpressionControlOption<T> {
  return {
    value: value,
    expression: expressionString,
    requiredImport: requiredImport,
  }
}

export function importStar(source: string, name: string): ImportType {
  return {
    source: source,
    name: name,
    type: 'star',
  }
}
export function importDefault(source: string, name: string): ImportType {
  return {
    source: source,
    name: name,
    type: 'default',
  }
}
export function importNamed(source: string, name: string): ImportType {
  return {
    source: source,
    name: name,
    type: null,
  }
}
