import { fastForEach } from '../utils'

export type PropertyControls<ComponentProps = any> = {
  [K in keyof ComponentProps]?: ControlDescription<Partial<ComponentProps>>
}

// Please ensure that `property-controls-utils.ts` is kept up to date
// with any changes to this or the component types.
export type ControlDescription<P = any> =
  | NumberControlDescription<P>
  | EnumControlDescription<P>
  | BooleanControlDescription<P>
  | StringControlDescription<P>
  | ColorControlDescription<P>
  | FusedNumberControlDescription<P>
  | ImageControlDescription<P>
  | FileControlDescription<P>
  | ComponentInstanceDescription<P>
  | ArrayControlDescription<P>
  | EventHandlerControlDescription<P>
  | SliderControlDescription<P>
  | PopUpListControlDescription<P>
  | OptionsControlDescription<P>
  | IgnoreControlDescription<P>

interface BaseControlDescription<P = any> {
  title?: string
}

export interface NumberControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Number | 'number'
  defaultValue?: number
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

export interface EnumControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Enum | 'enum'
  defaultValue?: string | boolean | number | undefined | null
  options: (string | boolean | number | undefined | null)[]
  optionTitles?: string[] | ((props: P | null) => string[])
  displaySegmentedControl?: boolean
}

export interface BooleanControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Boolean | 'boolean'
  defaultValue?: boolean
  disabledTitle?: string
  enabledTitle?: string
}

export interface StringControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.String | 'string'
  defaultValue?: string
  placeholder?: string
  obscured?: boolean
}

export interface ColorControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Color | 'color'
  defaultValue?: string
}

export interface FusedNumberControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.FusedNumber | 'fusednumber'
  defaultValue?: number
  toggleKey: keyof P
  toggleTitles: [string, string]
  valueKeys: [keyof P, keyof P, keyof P, keyof P]
  valueLabels: [string, string, string, string]
  min?: number
}

export interface ImageControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Image | 'image'
}

export interface FileControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.File | 'file'
  allowedFileTypes: string[]
}

export interface ComponentInstanceDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.ComponentInstance | 'componentinstance'
}

export interface ArrayControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Array | 'array'
  propertyControl: ControlDescription<P>
  maxCount?: number
  defaultValue?: any[]
}

export interface EventHandlerControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.EventHandler | 'eventhandler'
}

export interface SliderControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Slider | 'slider'
  defaultValue?: number
  min: number
  max: number
  step: number
}

export interface PopUpListControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.PopUpList | 'popuplist'
  defaultValue?: any
  options: Array<{
    value: any
    label: string
  }>
}

export interface OptionsControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Options | 'options'
  defaultValue?: any
  options: Array<{
    value: any
    label: string
  }>
}

export interface IgnoreControlDescription<P = any> extends BaseControlDescription<P> {
  type: ControlType.Ignore | 'ignore'
}

export enum ControlType {
  Boolean = 'boolean',
  Number = 'number',
  String = 'string',
  FusedNumber = 'fusednumber',
  Enum = 'enum',
  Color = 'color',
  Image = 'image',
  File = 'file',
  ComponentInstance = 'componentinstance',
  Array = 'array',
  EventHandler = 'eventhandler',
  Slider = 'slider',
  PopUpList = 'popuplist',
  Options = 'options',
  Ignore = 'ignore',
}

export function addPropertyControls(component: any, propertyControls: PropertyControls): void {
  component['propertyControls'] = propertyControls
}

export function getDefaultProps(propertyControls: PropertyControls): { [prop: string]: unknown } {
  let defaults: { [prop: string]: unknown } = {}
  const propKeys = Object.keys(propertyControls)
  fastForEach(propKeys, (prop) => {
    const control = propertyControls[prop]
    if (control != null && (control as any).defaultValue != null) {
      defaults[prop] = (control as any).defaultValue
    }
  })
  return defaults
}
