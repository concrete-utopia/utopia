import type { CSSProperties } from 'react'
import { fastForEach } from '../utils'

// Base Level Controls

export type BaseControlType =
  | 'boolean'
  | 'color'
  | 'componentinstance'
  | 'enum'
  | 'expression-enum'
  | 'eventhandler'
  | 'ignore'
  | 'image'
  | 'number'
  | 'options'
  | 'popuplist'
  | 'string'
  | 'styleobject'
  | 'vector2'
  | 'vector3'

interface AbstractControlDescription<T extends ControlType> {
  title?: string
  type: T
  defaultValue?: unknown
}

interface AbstractBaseControlDescription<T extends BaseControlType>
  extends AbstractControlDescription<T> {}

export interface BooleanControlDescription extends AbstractBaseControlDescription<'boolean'> {
  defaultValue?: boolean
  disabledTitle?: string
  enabledTitle?: string
}

export interface ColorControlDescription extends AbstractBaseControlDescription<'color'> {
  defaultValue?: string
}

export interface ComponentInstanceDescription
  extends AbstractBaseControlDescription<'componentinstance'> {
  defaultValue?: never
}

export type AllowedEnumType = string | boolean | number | undefined | null

export interface EnumControlDescription extends AbstractBaseControlDescription<'enum'> {
  defaultValue?: AllowedEnumType
  options: AllowedEnumType[]
  optionTitles?: string[] | ((props: unknown | null) => string[])
  displaySegmentedControl?: boolean
}

export interface ImportType {
  source: string // importSource
  name: string
  type: 'star' | 'default' | null
}

export interface ExpressionEnum {
  value: AllowedEnumType
  expression: string
  import?: ImportType
}

export interface ExpressionEnumControlDescription
  extends AbstractBaseControlDescription<'expression-enum'> {
  defaultValue?: ExpressionEnum
  options: ExpressionEnum[]
  optionTitles?: string[] | ((props: unknown | null) => string[])
}

export interface EventHandlerControlDescription
  extends AbstractBaseControlDescription<'eventhandler'> {
  defaultValue?: never
}

export interface IgnoreControlDescription extends AbstractBaseControlDescription<'ignore'> {
  defaultValue?: never
}

export interface ImageControlDescription extends AbstractBaseControlDescription<'image'> {
  defaultValue?: string
}

export interface NumberControlDescription extends AbstractBaseControlDescription<'number'> {
  defaultValue?: number | null
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export interface OptionsControlDescription extends AbstractBaseControlDescription<'options'> {
  defaultValue?: unknown
  options: Array<{
    value: unknown
    label: string
  }>
}

export interface PopUpListControlDescription extends AbstractBaseControlDescription<'popuplist'> {
  defaultValue?: unknown
  options: Array<{
    value: unknown
    label: string
  }>
}

export interface StringControlDescription extends AbstractBaseControlDescription<'string'> {
  defaultValue?: string
  placeholder?: string
  obscured?: boolean
}

export interface StyleObjectControlDescription
  extends AbstractBaseControlDescription<'styleobject'> {
  defaultValue?: CSSProperties
  placeholder?: CSSProperties
}
export interface Vector2ControlDescription extends AbstractBaseControlDescription<'vector2'> {
  defaultValue?: [number, number]
}

export interface Vector3ControlDescription extends AbstractBaseControlDescription<'vector3'> {
  defaultValue?: [number, number, number]
}

export type BaseControlDescription =
  | BooleanControlDescription
  | ColorControlDescription
  | ComponentInstanceDescription
  | EnumControlDescription
  | ExpressionEnumControlDescription
  | EventHandlerControlDescription
  | IgnoreControlDescription
  | ImageControlDescription
  | NumberControlDescription
  | OptionsControlDescription
  | PopUpListControlDescription
  | StringControlDescription
  | StyleObjectControlDescription
  | Vector2ControlDescription
  | Vector3ControlDescription

// Higher Level Controls

export type HigherLevelControlType = 'array' | 'object' | 'union'

export type ControlType = BaseControlType | HigherLevelControlType | 'folder'

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
  type: 'folder'
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
  switch (control.type) {
    case 'boolean':
    case 'color':
    case 'componentinstance':
    case 'enum':
    case 'expression-enum':
    case 'eventhandler':
    case 'ignore':
    case 'image':
    case 'number':
    case 'options':
    case 'popuplist':
    case 'string':
    case 'styleobject':
    case 'vector2':
    case 'vector3':
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
  switch (control.type) {
    case 'boolean':
    case 'color':
    case 'componentinstance':
    case 'enum':
    case 'expression-enum':
    case 'eventhandler':
    case 'ignore':
    case 'image':
    case 'number':
    case 'options':
    case 'popuplist':
    case 'string':
    case 'styleobject':
    case 'vector2':
    case 'vector3':
    case 'folder':
      return false
    case 'array':
    case 'object':
    case 'union':
      return true
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
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

export function expression(
  value: AllowedEnumType,
  expressionString: string,
  toImport?: ImportType,
): ExpressionEnum {
  return {
    value: value,
    expression: expressionString,
    import: toImport,
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
