const Button = {
  href: {
    type: 'string',
    title: 'href',
  },
  onClick: {
    type: 'eventhandler',
    title: 'onClick',
  },
  disabled: {
    type: 'boolean',
    title: 'disabled',
    defaultValue: false,
  },
  size: {
    type: 'enum',
    title: 'size',
    options: ['large', 'medium', 'small'],
  },
  type: {
    type: 'enum',
    title: 'type',
    options: ['primary', 'ghost', 'dashed', 'danger', 'link', 'text', 'default'],
    defaultValue: 'default',
  },
  shape: {
    type: 'enum',
    title: 'shape',
    options: ['circle', 'round'],
  },
  icon: {
    type: 'componentinstance',
    title: 'icon',
  },
  ghost: {
    type: 'boolean',
    title: 'ghost',
    defaultValue: false,
  },
  block: {
    type: 'boolean',
    title: 'block',
    defaultValue: false,
  },
  danger: {
    type: 'boolean',
    title: 'danger',
    defaultValue: false,
  },
  loading: {
    type: 'boolean',
    title: 'loading',
    defaultValue: false,
  },
  htmlType: {
    type: 'string',
    title: 'htmlType',
  },
  target: {
    type: 'string',
    title: 'target',
    defaultValue: 'button',
  },
}
export const AntdControlsVersion = '4.3.4'
export const AntdControls = {
  Button: Button,
}
