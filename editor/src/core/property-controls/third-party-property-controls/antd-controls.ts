import { PropertyControls } from 'utopia-api'

const Button: PropertyControls = {
  href: {
    type: 'string',
    title: 'href',
  },
  onClick: {
    type: 'ignore',
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
    options: ['circle', 'round', 'default'],
    defaultValue: 'default',
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

const Space: PropertyControls = {
  align: {
    type: 'enum',
    title: 'align',
    options: ['start', 'end', 'center', 'baseline'],
    defaultValue: 'center',
  },
  direction: {
    type: 'enum',
    title: 'direction',
    options: ['vertical', 'horizontal'],
    defaultValue: 'vertical',
  },
  size: {
    type: 'enum', // or a number
    title: 'size',
    options: ['small', 'middle', 'large'],
    defaultValue: 'middle',
  },
}

const Row: PropertyControls = {
  align: {
    type: 'enum',
    title: 'align',
    options: ['top', 'middle', 'bottom'],
    defaultValue: 'top',
  },
  gutter: {
    //spacing between grids, could be a number or a object like { xs: 8, sm: 16, md: 24}. or you can use array to make horizontal and vertical spacing work at the same time [horizontal, vertical]
    type: 'number',
    title: 'gutter',
    defaultValue: 0,
  },
  justify: {
    type: 'enum',
    title: 'justify',
    options: ['start', 'end', 'center', 'space-around', 'space-between'],
    defaultValue: 'center',
  },
}

const Col: PropertyControls = {
  flex: {
    type: 'number', // or string
    title: 'flex',
    defaultValue: 1,
  },
  offset: {
    type: 'number',
    title: 'offset',
    defaultValue: 0,
  },
  order: {
    type: 'number',
    title: 'order',
    defaultValue: 0,
  },
  pull: {
    type: 'number',
    title: 'pull',
    defaultValue: 0,
  },
  push: {
    type: 'number',
    title: 'push',
    defaultValue: 0,
  },
  span: {
    type: 'number',
    title: 'span',
  },
  xs: {
    type: 'number', // or an object containing above props
    title: 'xs',
    defaultValue: 0,
  },
  sm: {
    type: 'number', // or an object containing above props
    title: 'sm',
    defaultValue: 0,
  },
  md: {
    type: 'number', // or an object containing above props
    title: 'md',
    defaultValue: 0,
  },
  lg: {
    type: 'number', // or an object containing above props
    title: 'lg',
    defaultValue: 0,
  },
  xl: {
    type: 'number', // or an object containing above props
    title: 'xl',
    defaultValue: 0,
  },
  xxl: {
    type: 'number', // or an object containing above props
    title: 'xxl',
    defaultValue: 0,
  },
}

const Text: PropertyControls = {
  code: {
    type: 'boolean',
    title: 'code',
    defaultValue: false,
  },
  copyable: {
    type: 'boolean', // or { text: string, onCopy: Function }
    title: 'copyable',
    defaultValue: false,
  },
  delete: {
    type: 'boolean',
    title: 'delete',
    defaultValue: false,
  },
  disabled: {
    type: 'boolean',
    title: 'disabled',
    defaultValue: false,
  },
  editable: {
    type: 'boolean', // or { editing: boolean, onStart: Function, onChange: Function(string) }
    title: 'editable',
    defaultValue: false,
  },
  ellipsis: {
    type: 'boolean',
    title: 'ellipsis',
    defaultValue: false,
  },
  mark: {
    type: 'boolean',
    title: 'mark',
    defaultValue: false,
  },
  keyboard: {
    type: 'boolean',
    title: 'keyboard',
    defaultValue: false,
  },
  underline: {
    type: 'boolean',
    title: 'underline',
    defaultValue: false,
  },
  strong: {
    type: 'boolean',
    title: 'strong',
    defaultValue: false,
  },
  type: {
    type: 'enum',
    title: 'type',
    options: ['secondary', 'warning', 'danger'],
  },
}

const Menu: PropertyControls = {
  defaultOpenKeys: {
    type: 'ignore',
    title: 'defaultOpenKeys',
  },
  defaultSelectedKeys: {
    type: 'ignore',
    title: 'defaultSelectedKeys',
  },
  forceSubMenuRender: {
    type: 'boolean',
    title: 'forceSubMenuRender',
    defaultValue: false,
  },
  inlineCollapsed: {
    type: 'boolean',
    title: 'inlineCollapsed',
    defaultValue: false,
  },
  inlineIndent: {
    type: 'number',
    title: 'inlineIndent',
    defaultValue: 24,
  },
  mode: {
    type: 'enum',
    title: 'mode',
    options: ['vertical', 'horizontal', 'inline'],
    defaultValue: 'inline',
  },
  multiple: {
    type: 'boolean',
    title: 'multiple',
    defaultValue: false,
  },
  openKeys: {
    type: 'ignore',
    title: 'openKeys',
  },
  selectable: {
    type: 'boolean',
    title: 'selectable',
    defaultValue: true,
  },
  selectedKeys: {
    type: 'ignore',
    title: 'selectedKeys',
  },
  subMenuCloseDelay: {
    type: 'number',
    title: 'subMenuCloseDelay',
    defaultValue: 0.1,
  },
  subMenuOpenDelay: {
    type: 'number',
    title: 'subMenuOpenDelay',
    defaultValue: 0,
  },
  theme: {
    type: 'enum',
    title: 'theme',
    options: ['light', 'dark'],
    defaultValue: 'light',
  },
  onClick: {
    // function({ item, key, keyPath, domEvent })
    type: 'ignore',
    title: 'onClick',
  },
  onDeselect: {
    // function({ item, key, keyPath, selectedKeys, domEvent })
    type: 'ignore',
    title: 'onClick',
  },
  onOpenChange: {
    // function(openKeys: string[])
    type: 'ignore',
    title: 'onClick',
  },
  onSelect: {
    // function({ item, key, keyPath, selectedKeys, domEvent })
    type: 'ignore',
    title: 'onClick',
  },
  overflowedIndicator: {
    type: 'componentinstance',
    title: 'overflowedIndicator',
  },
}

const MenuItem: PropertyControls = {
  disabled: {
    type: 'boolean',
    title: 'disabled',
    defaultValue: false,
  },
  key: {
    type: 'string',
    title: 'key',
  },
  title: {
    type: 'string',
    title: 'title',
  },
  icon: {
    type: 'componentinstance',
    title: 'icon',
  },
  danger: {
    type: 'boolean',
    title: 'danger',
    defaultValue: false,
  },
}

const MenuSubMenu: PropertyControls = {
  popupClassName: {
    type: 'string',
    title: 'popupClassName',
  },
  disabled: {
    type: 'boolean',
    title: 'disabled',
    defaultValue: false,
  },
  key: {
    type: 'string',
    title: 'key',
  },
  title: {
    type: 'string',
    title: 'title',
  },
  icon: {
    type: 'componentinstance',
    title: 'icon',
  },
  onTitleClick: {
    // function({ key, domEvent })
    type: 'ignore',
    title: 'onTitleClick',
  },
}

const MenuItemGroup: PropertyControls = {
  title: {
    type: 'string',
    title: 'title',
  },
}

export const AntdControls = {
  Button: Button,
  Space: Space,
  Row: Row,
  Col: Col,
  'Typography.Text': Text,
  Menu: Menu,
  'Menu.Item': MenuItem,
  'Menu.SubMenu': MenuSubMenu,
  'Menu.ItemGroup': MenuItemGroup,
}
