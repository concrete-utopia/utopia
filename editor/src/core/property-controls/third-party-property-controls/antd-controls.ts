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
  ghost: {
    type: 'boolean',
  },
  block: {
    type: 'boolean',
  },
  danger: {
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
}

export const AntdControls = {
  Button: Button,
}
