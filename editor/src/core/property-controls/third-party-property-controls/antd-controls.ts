import { ControlType } from 'utopia-api'

const Button = {
  href: {
    type: ControlType.String,
    title: 'href',
  },
  onClick: {
    type: ControlType.EventHandler,
    title: 'onClick',
  },
  disabled: {
    type: ControlType.Boolean,
    title: 'disabled',
    defaultValue: false,
  },
  size: {
    type: ControlType.Enum,
    title: 'size',
    options: ['large', 'medium', 'small'],
  },
  type: {
    type: ControlType.Enum,
    title: 'type',
    options: ['primary', 'ghost', 'dashed', 'danger', 'link', 'text', 'default'],
    defaultValue: 'default',
  },
  shape: {
    type: ControlType.Enum,
    title: 'shape',
    options: ['circle', 'round'],
  },
  icon: {
    type: ControlType.ComponentInstance,
    title: 'icon',
  },
  ghost: {
    type: ControlType.Boolean,
    title: 'ghost',
    defaultValue: false,
  },
  block: {
    type: ControlType.Boolean,
    title: 'block',
    defaultValue: false,
  },
  danger: {
    type: ControlType.Boolean,
    title: 'danger',
    defaultValue: false,
  },
  loading: {
    type: ControlType.Boolean,
    title: 'loading',
    defaultValue: false,
  },
  htmlType: {
    type: ControlType.String,
    title: 'htmlType',
  },
  target: {
    type: ControlType.String,
    title: 'target',
    defaultValue: 'button',
  },
}

export const Space = {
  align: {
    type: ControlType.Enum,
    title: 'align',
    options: ['start', 'end', 'center', 'baseline'],
  },
  direction: {
    type: ControlType.Enum,
    title: 'direction',
    options: ['vertical', 'horizontal'],
  },
  size: {
    type: ControlType.Enum, // or a number
    title: 'size',
    options: ['small', 'middle', 'large'],
  },
}

export const Row = {
  align: {
    type: ControlType.Enum,
    title: 'align',
    options: ['top', 'middle', 'bottom'],
    defaultValue: 'top',
  },
  gutter: {
    //spacing between grids, could be a number or a object like { xs: 8, sm: 16, md: 24}. or you can use array to make horizontal and vertical spacing work at the same time [horizontal, vertical]
    type: ControlType.Number,
    title: 'gutter',
    defaultValue: 0,
  },
  justify: {
    type: ControlType.Enum,
    title: 'justify',
    options: ['start', 'end', 'center', 'space-around', 'space-between'],
  },
}

export const Col = {
  flex: {
    type: ControlType.Number, // or string
    title: 'flex',
  },
  offset: {
    type: ControlType.Number,
    title: 'offset',
    defaultValue: 0,
  },
  order: {
    type: ControlType.Number,
    title: 'order',
    defaultValue: 0,
  },
  pull: {
    type: ControlType.Number,
    title: 'pull',
    defaultValue: 0,
  },
  push: {
    type: ControlType.Number,
    title: 'push',
    defaultValue: 0,
  },
  span: {
    type: ControlType.Number,
    title: 'span',
  },
  xs: {
    type: ControlType.Number, // or an object containing above props
    title: 'xs',
  },
  sm: {
    type: ControlType.Number, // or an object containing above props
    title: 'sm',
  },
  md: {
    type: ControlType.Number, // or an object containing above props
    title: 'md',
  },
  lg: {
    type: ControlType.Number, // or an object containing above props
    title: 'lg',
  },
  xl: {
    type: ControlType.Number, // or an object containing above props
    title: 'xl',
  },
  xxl: {
    type: ControlType.Number, // or an object containing above props
    title: 'xxl',
  },
}

export const Text = {
  code: {
    type: ControlType.Boolean,
    title: 'code',
    defaultValue: false,
  },
  copyable: {
    type: ControlType.Boolean, // or { text: string, onCopy: Function }
    title: 'copyable',
    defaultValue: false,
  },
  delete: {
    type: ControlType.Boolean,
    title: 'delete',
    defaultValue: false,
  },
  disabled: {
    type: ControlType.Boolean,
    title: 'disabled',
    defaultValue: false,
  },
  editable: {
    type: ControlType.Boolean, // or { editing: boolean, onStart: Function, onChange: Function(string) }
    title: 'editable',
    defaultValue: false,
  },
  ellipsis: {
    type: ControlType.Boolean,
    title: 'ellipsis',
    defaultValue: false,
  },
  mark: {
    type: ControlType.Boolean,
    title: 'mark',
    defaultValue: false,
  },
  keyboard: {
    type: ControlType.Boolean,
    title: 'keyboard',
    defaultValue: false,
  },
  underline: {
    type: ControlType.Boolean,
    title: 'underline',
    defaultValue: false,
  },
  strong: {
    type: ControlType.Boolean,
    title: 'strong',
    defaultValue: false,
  },
  type: {
    type: ControlType.Enum,
    title: 'type',
    options: ['secondary', 'warning', 'danger'],
  },
}

export const AntdControlsVersion = '4.3.4'
export const AntdControls = {
  Button: Button,
  Space: Space,
  Row: Row,
  Col: Col,
  'Typography.Text': Text,
}
