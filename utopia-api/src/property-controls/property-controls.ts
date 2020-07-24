import type { CSSProperties } from 'react'
import { fastForEach } from '../utils'

// Base Level Controls

export type BaseControlType =
  | 'boolean'
  | 'color'
  | 'componentinstance'
  | 'enum'
  | 'eventhandler'
  | 'ignore'
  | 'image'
  | 'number'
  | 'options'
  | 'popuplist'
  | 'slider'
  | 'string'
  | 'style-object'

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
  defaultValue?: number
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

export interface SliderControlDescription extends AbstractBaseControlDescription<'slider'> {
  defaultValue?: number
  max: number
  min: number
  step: number
}

export interface StringControlDescription extends AbstractBaseControlDescription<'string'> {
  defaultValue?: string
  placeholder?: string
  obscured?: boolean
}

export interface StyleObjectControlDescription
  extends AbstractBaseControlDescription<'style-object'> {
  defaultValue?: CSSProperties
  placeholder?: CSSProperties
}

export type BaseControlDescription =
  | BooleanControlDescription
  | ColorControlDescription
  | ComponentInstanceDescription
  | EnumControlDescription
  | EventHandlerControlDescription
  | IgnoreControlDescription
  | ImageControlDescription
  | NumberControlDescription
  | OptionsControlDescription
  | PopUpListControlDescription
  | SliderControlDescription
  | StringControlDescription
  | StyleObjectControlDescription

// Higher Level Controls

export type HigherLevelControlType = 'array' | 'object' | 'union'

export type ControlType = BaseControlType | HigherLevelControlType

interface AbstractHigherLevelControlDescription<T extends HigherLevelControlType>
  extends AbstractControlDescription<T> {}

export interface ArrayControlDescription extends AbstractHigherLevelControlDescription<'array'> {
  defaultValue?: unknown[]
  propertyControl: ControlDescription
  maxCount?: number
}

export interface ObjectControlDescription extends AbstractHigherLevelControlDescription<'object'> {
  defaultValue?: unknown
  object: { [prop: string]: ControlDescription }
}

export interface UnionControlDescription extends AbstractHigherLevelControlDescription<'union'> {
  defaultValue?: unknown
  controls: Array<ControlDescription>
}

export type HigherLevelControlDescription =
  | ArrayControlDescription
  | ObjectControlDescription
  | UnionControlDescription

// Please ensure that `property-controls-utils.ts` is kept up to date
// with any changes to this or the component types.
export type ControlDescription = BaseControlDescription | HigherLevelControlDescription

export function isBaseControlDescription(
  control: ControlDescription,
): control is BaseControlDescription {
  switch (control.type) {
    case 'boolean':
    case 'color':
    case 'componentinstance':
    case 'enum':
    case 'eventhandler':
    case 'ignore':
    case 'image':
    case 'number':
    case 'options':
    case 'popuplist':
    case 'slider':
    case 'string':
    case 'style-object':
      return true
    case 'array':
    case 'object':
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
  switch (control.type) {
    case 'boolean':
    case 'color':
    case 'componentinstance':
    case 'enum':
    case 'eventhandler':
    case 'ignore':
    case 'image':
    case 'number':
    case 'options':
    case 'popuplist':
    case 'slider':
    case 'string':
    case 'style-object':
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

export type PropertyControls<ComponentProps = any> = {
  [K in keyof ComponentProps]?: ControlDescription
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
    if (control != null && control.defaultValue != null) {
      defaults[prop] = control.defaultValue
    }
  })
  return defaults
}
