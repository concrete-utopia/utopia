import { PropertyControls } from 'utopia-api'

const Button: PropertyControls = {
  href: {
    control: 'string',
    label: 'href',
  },
  onClick: {
    control: 'rawjs',
    label: 'onClick',
  },
  disabled: {
    control: 'boolean',
    label: 'disabled',
    defaultValue: false,
  },
  size: {
    control: 'enum',
    label: 'size',
    options: ['large', 'medium', 'small'],
  },
  type: {
    control: 'enum',
    label: 'type',
    options: ['primary', 'ghost', 'dashed', 'danger', 'link', 'text', 'default'],
    defaultValue: 'default',
  },
  shape: {
    control: 'enum',
    label: 'shape',
    options: ['circle', 'round', 'default'],
    defaultValue: 'default',
  },
  icon: {
    control: 'rawjs',
    label: 'icon',
  },
  ghost: {
    control: 'boolean',
    label: 'ghost',
    defaultValue: false,
  },
  block: {
    control: 'boolean',
    label: 'block',
    defaultValue: false,
  },
  danger: {
    control: 'boolean',
    label: 'danger',
    defaultValue: false,
  },
  loading: {
    control: 'boolean',
    label: 'loading',
    defaultValue: false,
  },
  htmlType: {
    control: 'string',
    label: 'htmlType',
  },
  target: {
    control: 'string',
    label: 'target',
    defaultValue: 'button',
  },
}

const Space: PropertyControls = {
  align: {
    control: 'enum',
    label: 'align',
    options: ['start', 'end', 'center', 'baseline'],
    defaultValue: 'center',
  },
  direction: {
    control: 'enum',
    label: 'direction',
    options: ['vertical', 'horizontal'],
    defaultValue: 'vertical',
  },
  size: {
    control: 'enum', // or a number
    label: 'size',
    options: ['small', 'middle', 'large'],
    defaultValue: 'middle',
  },
}

const Row: PropertyControls = {
  align: {
    control: 'enum',
    label: 'align',
    options: ['top', 'middle', 'bottom'],
    defaultValue: 'top',
  },
  gutter: {
    //spacing between grids, could be a number or a object like { xs: 8, sm: 16, md: 24}. or you can use array to make horizontal and vertical spacing work at the same time [horizontal, vertical]
    control: 'number',
    label: 'gutter',
    defaultValue: 0,
  },
  justify: {
    control: 'enum',
    label: 'justify',
    options: ['start', 'end', 'center', 'space-around', 'space-between'],
    defaultValue: 'center',
  },
}

const Col: PropertyControls = {
  flex: {
    control: 'number', // or string
    label: 'flex',
    defaultValue: 1,
  },
  offset: {
    control: 'number',
    label: 'offset',
    defaultValue: 0,
  },
  order: {
    control: 'number',
    label: 'order',
    defaultValue: 0,
  },
  pull: {
    control: 'number',
    label: 'pull',
    defaultValue: 0,
  },
  push: {
    control: 'number',
    label: 'push',
    defaultValue: 0,
  },
  span: {
    control: 'number',
    label: 'span',
  },
  xs: {
    control: 'number', // or an object containing above props
    label: 'xs',
    defaultValue: 0,
  },
  sm: {
    control: 'number', // or an object containing above props
    label: 'sm',
    defaultValue: 0,
  },
  md: {
    control: 'number', // or an object containing above props
    label: 'md',
    defaultValue: 0,
  },
  lg: {
    control: 'number', // or an object containing above props
    label: 'lg',
    defaultValue: 0,
  },
  xl: {
    control: 'number', // or an object containing above props
    label: 'xl',
    defaultValue: 0,
  },
  xxl: {
    control: 'number', // or an object containing above props
    label: 'xxl',
    defaultValue: 0,
  },
}

const Text: PropertyControls = {
  code: {
    control: 'boolean',
    label: 'code',
    defaultValue: false,
  },
  copyable: {
    control: 'boolean', // or { text: string, onCopy: Function }
    label: 'copyable',
    defaultValue: false,
  },
  delete: {
    control: 'boolean',
    label: 'delete',
    defaultValue: false,
  },
  disabled: {
    control: 'boolean',
    label: 'disabled',
    defaultValue: false,
  },
  editable: {
    control: 'boolean', // or { editing: boolean, onStart: Function, onChange: Function(string) }
    label: 'editable',
    defaultValue: false,
  },
  ellipsis: {
    control: 'boolean',
    label: 'ellipsis',
    defaultValue: false,
  },
  mark: {
    control: 'boolean',
    label: 'mark',
    defaultValue: false,
  },
  keyboard: {
    control: 'boolean',
    label: 'keyboard',
    defaultValue: false,
  },
  underline: {
    control: 'boolean',
    label: 'underline',
    defaultValue: false,
  },
  strong: {
    control: 'boolean',
    label: 'strong',
    defaultValue: false,
  },
  type: {
    control: 'enum',
    label: 'type',
    options: ['secondary', 'warning', 'danger'],
  },
}

const Menu: PropertyControls = {
  defaultOpenKeys: {
    control: 'array',
    propertyControl: {
      control: 'string',
    },
    label: 'defaultOpenKeys',
  },
  defaultSelectedKeys: {
    control: 'ignore',
    label: 'defaultSelectedKeys',
  },
  forceSubMenuRender: {
    control: 'boolean',
    label: 'forceSubMenuRender',
    defaultValue: false,
  },
  inlineCollapsed: {
    control: 'boolean',
    label: 'inlineCollapsed',
    defaultValue: false,
  },
  inlineIndent: {
    control: 'number',
    label: 'inlineIndent',
    defaultValue: 24,
  },
  mode: {
    control: 'enum',
    label: 'mode',
    options: ['vertical', 'horizontal', 'inline'],
    defaultValue: 'inline',
  },
  multiple: {
    control: 'boolean',
    label: 'multiple',
    defaultValue: false,
  },
  openKeys: {
    control: 'ignore',
    label: 'openKeys',
  },
  selectable: {
    control: 'boolean',
    label: 'selectable',
    defaultValue: true,
  },
  selectedKeys: {
    control: 'ignore',
    label: 'selectedKeys',
  },
  subMenuCloseDelay: {
    control: 'number',
    label: 'subMenuCloseDelay',
    defaultValue: 0.1,
  },
  subMenuOpenDelay: {
    control: 'number',
    label: 'subMenuOpenDelay',
    defaultValue: 0,
  },
  theme: {
    control: 'enum',
    label: 'theme',
    options: ['light', 'dark'],
    defaultValue: 'light',
  },
  onClick: {
    // function({ item, key, keyPath, domEvent })
    control: 'ignore',
    label: 'onClick',
  },
  onDeselect: {
    // function({ item, key, keyPath, selectedKeys, domEvent })
    control: 'ignore',
    label: 'onClick',
  },
  onOpenChange: {
    // function(openKeys: string[])
    control: 'ignore',
    label: 'onClick',
  },
  onSelect: {
    // function({ item, key, keyPath, selectedKeys, domEvent })
    control: 'ignore',
    label: 'onClick',
  },
  overflowedIndicator: {
    control: 'rawjs',
    label: 'overflowedIndicator',
  },
}

const MenuItem: PropertyControls = {
  disabled: {
    control: 'boolean',
    label: 'disabled',
    defaultValue: false,
  },
  key: {
    control: 'string',
    label: 'key',
  },
  label: {
    control: 'string',
    label: 'title',
  },
  icon: {
    control: 'rawjs',
    label: 'icon',
  },
  danger: {
    control: 'boolean',
    label: 'danger',
    defaultValue: false,
  },
}

const MenuSubMenu: PropertyControls = {
  popupClassName: {
    control: 'string',
    label: 'popupClassName',
  },
  disabled: {
    control: 'boolean',
    label: 'disabled',
    defaultValue: false,
  },
  key: {
    control: 'string',
    label: 'key',
  },
  title: {
    control: 'string',
    label: 'title',
  },
  icon: {
    control: 'rawjs',
    label: 'icon',
  },
  onTitleClick: {
    // function({ key, domEvent })
    control: 'ignore',
    label: 'onTitleClick',
  },
}

const MenuItemGroup: PropertyControls = {
  title: {
    control: 'string',
    label: 'title',
  },
}

export const AntdControls = {
  Button: Button,
  Space: Space,
  Row: Row,
  Col: Col,
  Text: Text,
  Menu: Menu,
  Item: MenuItem,
  SubMenu: MenuSubMenu,
  ItemGroup: MenuItemGroup,
}
