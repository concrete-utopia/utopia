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
type DefaultValueType<T extends ControlType> = T extends 'array'
  ? unknown[]
  : T extends 'boolean'
  ? boolean
  : T extends 'color'
  ? string
  : T extends 'enum'
  ? string | boolean | number | undefined | null
  : T extends 'ignore'
  ? never
  : T extends 'image'
  ? string
  : T extends 'number'
  ? number
  : T extends 'object'
  ? unknown
  : T extends 'options'
  ? unknown
  : T extends 'popuplist'
  ? unknown
  : T extends 'slider'
  ? number
  : T extends 'string'
  ? string
  : T extends 'union'
  ? unknown
  : never

interface BaseControlDescription<T extends ControlType> {
  title?: string
  type: T
  defaultValue?: DefaultValueType<T>
}

export interface ArrayControlDescription extends BaseControlDescription<'array'> {
  propertyControl: ControlDescription
  maxCount?: number
}

export interface BooleanControlDescription extends BaseControlDescription<'boolean'> {
  disabledTitle?: string
  enabledTitle?: string
}

export interface ColorControlDescription extends BaseControlDescription<'color'> {}

export interface ComponentInstanceDescription extends BaseControlDescription<'componentinstance'> {}

export interface EnumControlDescription extends BaseControlDescription<'enum'> {
  options: (string | boolean | number | undefined | null)[]
  optionTitles?: string[] | ((props: unknown | null) => string[])
  displaySegmentedControl?: boolean
}

export interface EventHandlerControlDescription extends BaseControlDescription<'eventhandler'> {}

export interface IgnoreControlDescription extends BaseControlDescription<'ignore'> {}

export interface ImageControlDescription extends BaseControlDescription<'image'> {}

export interface NumberControlDescription extends BaseControlDescription<'number'> {
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export interface ObjectControlDescription extends BaseControlDescription<'object'> {
  object?: { [prop: string]: ControlDescription }
}

export interface OptionsControlDescription extends BaseControlDescription<'options'> {
  options: Array<{
    value: unknown
    label: string
  }>
}

export interface PopUpListControlDescription extends BaseControlDescription<'popuplist'> {
  options: Array<{
    value: unknown
    label: string
  }>
}

export interface SliderControlDescription extends BaseControlDescription<'slider'> {
  max: number
  min: number
  step: number
}

export interface StringControlDescription extends BaseControlDescription<'string'> {
  placeholder?: string
  obscured?: boolean
}

export interface UnionControlDescription extends BaseControlDescription<'union'> {
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
