type PrimitiveControlType = 'boolean' | 'number' | 'string' | 'object' | 'array'
type ControlType = PrimitiveControlType | PrimitiveControlType[]
type DefaultValueType<T extends ControlType> = T extends 'boolean'
  ? boolean
  : T extends 'number'
  ? number
  : T extends 'string'
  ? string
  : T extends 'object'
  ? unknown
  : T extends 'array'
  ? unknown[]
  : T extends PrimitiveControlType[]
  ? boolean | number | string | unknown | unknown[]
  : never

interface BaseControlDescription<T extends ControlType> {
  title?: string
  type: T
  defaultValue?: DefaultValueType<T>
}

interface BaseBooleanControlDescription<T extends ControlType> extends BaseControlDescription<T> {
  disabledTitle?: string
  enabledTitle?: string
}

interface BaseNumberControlDescription<T extends ControlType> extends BaseControlDescription<T> {
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
}

interface BaseStringControlDescription<T extends ControlType> extends BaseControlDescription<T> {
  placeholder?: string
  obscured?: boolean
}

interface BaseObjectControlDescription<T extends ControlType> extends BaseControlDescription<T> {
  object?: { [prop: string]: ControlDescription }
}

interface BaseArrayControlDescription<T extends ControlType> extends BaseControlDescription<T> {
  array?: ControlDescription[]
}

export interface BooleanControlDescription extends BaseBooleanControlDescription<'boolean'> {}
export interface NumberControlDescription extends BaseNumberControlDescription<'number'> {}
export interface StringControlDescription extends BaseStringControlDescription<'string'> {}
export interface ObjectControlDescription extends BaseObjectControlDescription<'object'> {}
export interface ArrayControlDescription extends BaseArrayControlDescription<'array'> {}

export interface UnionControlDescription
  extends BaseBooleanControlDescription<PrimitiveControlType[]>,
    BaseNumberControlDescription<PrimitiveControlType[]>,
    BaseStringControlDescription<PrimitiveControlType[]>,
    BaseObjectControlDescription<PrimitiveControlType[]>,
    BaseArrayControlDescription<PrimitiveControlType[]> {}

export type ControlDescription =
  | BooleanControlDescription
  | NumberControlDescription
  | StringControlDescription
  | ObjectControlDescription
  | ArrayControlDescription
  | UnionControlDescription

const simpleBoolean: ControlDescription = {
  title: 'thing',
  type: 'boolean',
  defaultValue: false,
}

const bgSize: ControlDescription = {
  title: 'Background Size',
  type: ['string', 'number'],
  defaultValue: 'auto',
  min: 0,
  max: 100,
  step: 1,
}

const rgb: ControlDescription = {
  title: 'rgb',
  type: 'object',
  object: {
    red: {
      title: 'red',
      type: 'number',
      defaultValue: 0,
    },
    green: {
      title: 'green',
      type: 'number',
      defaultValue: 0,
    },
    blue: {
      title: 'blue',
      type: 'number',
      defaultValue: 0,
    },
  },
}

const GutterObject: { [prop: string]: ControlDescription } = {
  xs: {
    title: 'xs',
    type: 'number',
    defaultValue: 8,
  },
  sm: {
    title: 'sm',
    type: 'number',
    defaultValue: 16,
  },
  md: {
    title: 'md',
    type: 'number',
    defaultValue: 24,
  },
  lg: {
    title: 'lg',
    type: 'number',
    defaultValue: 32,
  },
}

const BaseGutter: ControlDescription = {
  type: ['number', 'object'],
  object: GutterObject,
}

const Gutter: ControlDescription = {
  title: 'gutter',
  type: ['number', 'object', 'array'],
  defaultValue: 0,
  object: GutterObject,
  array: [BaseGutter, BaseGutter],
}
