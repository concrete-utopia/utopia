const Button = {
  href: {
    type: 'string',
  },
  onClick: {
    type: 'eventhandler',
  },
  disabled: {
    type: 'boolean',
  },
  size: {
    type: 'enum',
    options: ['large', 'medium', 'small'],
  },
  type: {
    type: 'enum',
    options: ['primary', 'ghost', 'dashed', 'danger', 'link', 'text', 'default'],
  },
  shape: {
    type: 'enum',
    options: ['circle', 'round'],
  },
  icon: {
    type: 'componentinstance',
  },
  ghost: {
    type: 'boolean',
  },
  block: {
    type: 'boolean',
  },
  danger: {
    type: 'boolean',
  },
  loading: {
    type: 'boolean',
  },
  htmlType: {
    type: 'string',
  },
  target: {
    type: 'string',
  },
}

export const AntdControls = {
  Button: Button,
}
