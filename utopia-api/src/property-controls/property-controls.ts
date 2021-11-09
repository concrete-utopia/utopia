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

export type CheckboxControlDescription = {
  control: 'checkbox'
  label?: string
  defaultValue?: boolean
  visibleByDefault?: boolean
  disabledTitle?: string
  enabledTitle?: string
}

export type ColorControlDescription = {
  control: 'color'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: string
}

export type AllowedEnumType = string | boolean | number | undefined | null
export type BasicControlOption<T> = {
  value: T
  label: string
}

export type BasicControlOptions<T> = AllowedEnumType[] | BasicControlOption<T>[]

export type PopUpListControlDescription = {
  control: 'popuplist'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: unknown
  options: BasicControlOptions<unknown>
}

export type ImportType = {
  source: string // importSource
  name: string | null
  type: 'star' | 'default' | null
}

export type ExpressionControlOption<T> = {
  value: T
  expression: string
  label?: string
  requiredImport?: ImportType
}

export type ExpressionPopUpListControlDescription = {
  control: 'expression-popuplist'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: ExpressionControlOption<unknown>
  options: ExpressionControlOption<unknown>[]
}

export type EulerControlDescription = {
  control: 'euler'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: [number, number, number, string]
}

export type NoneControlDescription = {
  control: 'none'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: never
}

export type Matrix3ControlDescription = {
  control: 'matrix3'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: [number, number, number, number, number, number, number, number, number]
}

export type Matrix4ControlDescription = {
  control: 'matrix4'
  label?: string
  visibleByDefault?: boolean
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

export type NumberInputControlDescription = {
  control: 'number-input'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: number | null
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export type RadioControlDescription = {
  control: 'radio'
  label?: string
  defaultValue?: unknown
  visibleByDefault?: boolean
  options: BasicControlOptions<unknown>
}

export type ExpressionInputControlDescription = {
  control: 'expression-input'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: unknown
}

export type StringInputControlDescription = {
  control: 'string-input'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: string
  placeholder?: string
  obscured?: boolean
}

export type StyleControlsControlDescription = {
  control: 'style-controls'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: CSSProperties
  placeholder?: CSSProperties
}

export type Vector2ControlDescription = {
  control: 'vector2'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: [number, number]
}

export type Vector3ControlDescription = {
  control: 'vector3'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: [number, number, number]
}

export type Vector4ControlDescription = {
  control: 'vector4'
  label?: string
  visibleByDefault?: boolean
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

export type HigherLevelControlType = 'array' | 'tuple' | 'object' | 'union'
export type RegularControlType = BaseControlType | HigherLevelControlType
export type ControlType = RegularControlType | 'folder'

export type ArrayControlDescription = {
  control: 'array'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: unknown[]
  propertyControl: RegularControlDescription
  maxCount?: number
}

export type ObjectControlDescription = {
  control: 'object'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: unknown
  object: { [prop: string]: RegularControlDescription }
}

export type UnionControlDescription = {
  control: 'union'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: unknown
  controls: Array<RegularControlDescription>
}
export type TupleControlDescription = {
  control: 'tuple'
  label?: string
  visibleByDefault?: boolean
  defaultValue?: unknown[]
  propertyControls: RegularControlDescription[]
}

export type FolderControlDescription = {
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

// Please ensure that `property-controls-utils.ts` is kept up to date
// with any changes to this or the component types.
export type ControlDescription = RegularControlDescription | FolderControlDescription

type ControlBaseFields = {
  control: RegularControlType
  label?: string
  defaultValue?: unknown
  visibleByDefault?: boolean
}
function getControlSharedFields(control: RegularControlDescription): ControlBaseFields {
  return {
    control: control.control,
    label: control.label,
    defaultValue: control.defaultValue,
    visibleByDefault: control.visibleByDefault,
  }
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
    case 'style-controls':
    case 'vector2':
    case 'vector3':
    case 'vector4':
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
