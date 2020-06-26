import { fastForEach } from '../utils'

type ControlType =
  | 'array'
  | 'boolean'
  | 'color'
  | 'componentinstance'
  | 'enum'
  | 'eventhandler'
  | 'ignore'
  | 'image'
  | 'number'
  | 'object'
  | 'options'
  | 'popuplist'
  | 'slider'
  | 'string'
  | 'union'

interface BaseControlDescription<T extends ControlType> {
  title?: string
  type: T
  defaultValue?: unknown
}

export interface ArrayControlDescription extends BaseControlDescription<'array'> {
  defaultValue?: unknown[]
  propertyControl: ControlDescription
  maxCount?: number
}

export interface BooleanControlDescription extends BaseControlDescription<'boolean'> {
  defaultValue?: boolean
  disabledTitle?: string
  enabledTitle?: string
}

export interface ColorControlDescription extends BaseControlDescription<'color'> {
  defaultValue?: string
}

export interface ComponentInstanceDescription extends BaseControlDescription<'componentinstance'> {
  defaultValue?: never
}

type AllowedEnumType = string | boolean | number | undefined | null

export interface EnumControlDescription extends BaseControlDescription<'enum'> {
  defaultValue?: AllowedEnumType
  options: AllowedEnumType[]
  optionTitles?: string[] | ((props: unknown | null) => string[])
  displaySegmentedControl?: boolean
}

export interface EventHandlerControlDescription extends BaseControlDescription<'eventhandler'> {
  defaultValue?: never
}

export interface IgnoreControlDescription extends BaseControlDescription<'ignore'> {
  defaultValue?: never
}

export interface ImageControlDescription extends BaseControlDescription<'image'> {
  defaultValue?: string
}

export interface NumberControlDescription extends BaseControlDescription<'number'> {
  defaultValue?: number
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export interface ObjectControlDescription extends BaseControlDescription<'object'> {
  defaultValue?: unknown
  object?: { [prop: string]: ControlDescription }
}

export interface OptionsControlDescription extends BaseControlDescription<'options'> {
  defaultValue?: unknown
  options: Array<{
    value: unknown
    label: string
  }>
}

export interface PopUpListControlDescription extends BaseControlDescription<'popuplist'> {
  defaultValue?: unknown
  options: Array<{
    value: unknown
    label: string
  }>
}

export interface SliderControlDescription extends BaseControlDescription<'slider'> {
  defaultValue?: number
  max: number
  min: number
  step: number
}

export interface StringControlDescription extends BaseControlDescription<'string'> {
  defaultValue?: string
  placeholder?: string
  obscured?: boolean
}

export interface UnionControlDescription extends BaseControlDescription<'union'> {
  defaultValue?: unknown
  controls: Array<ControlDescription>
}

// Please ensure that `property-controls-utils.ts` is kept up to date
// with any changes to this or the component types.
export type ControlDescription =
  | ArrayControlDescription
  | BooleanControlDescription
  | ColorControlDescription
  | ComponentInstanceDescription
  | EnumControlDescription
  | EventHandlerControlDescription
  | IgnoreControlDescription
  | ImageControlDescription
  | NumberControlDescription
  | ObjectControlDescription
  | OptionsControlDescription
  | PopUpListControlDescription
  | SliderControlDescription
  | StringControlDescription
  | UnionControlDescription

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
