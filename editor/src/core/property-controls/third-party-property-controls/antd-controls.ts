import { PropertyControls } from 'utopia-api'

const Button: PropertyControls = {
  href: {
    control: 'string-input',
    label: 'href',
  },
  onClick: {
    control: 'expression-input',
    label: 'onClick',
  },
  disabled: {
    control: 'checkbox',
    label: 'disabled',
    defaultValue: false,
  },
  size: {
    control: 'popuplist',
    label: 'size',
    options: ['large', 'medium', 'small'],
  },
  type: {
    control: 'popuplist',
    label: 'type',
    options: ['primary', 'ghost', 'dashed', 'danger', 'link', 'text', 'default'],
    defaultValue: 'default',
  },
  shape: {
    control: 'popuplist',
    label: 'shape',
    options: ['circle', 'round', 'default'],
    defaultValue: 'default',
  },
  icon: {
    control: 'expression-input',
    label: 'icon',
  },
  ghost: {
    control: 'checkbox',
    label: 'ghost',
    defaultValue: false,
  },
  block: {
    control: 'checkbox',
    label: 'block',
    defaultValue: false,
  },
  danger: {
    control: 'checkbox',
    label: 'danger',
    defaultValue: false,
  },
  loading: {
    control: 'checkbox',
    label: 'loading',
    defaultValue: false,
  },
  htmlType: {
    control: 'string-input',
    label: 'htmlType',
  },
  target: {
    control: 'string-input',
    label: 'target',
    defaultValue: 'button',
  },
}

const Space: PropertyControls = {
  align: {
    control: 'popuplist',
    label: 'align',
    options: ['start', 'end', 'center', 'baseline'],
    defaultValue: 'center',
  },
  direction: {
    control: 'popuplist',
    label: 'direction',
    options: ['vertical', 'horizontal'],
    defaultValue: 'vertical',
  },
  size: {
    control: 'popuplist', // or a number
    label: 'size',
    options: ['small', 'middle', 'large'],
    defaultValue: 'middle',
  },
}

const Row: PropertyControls = {
  align: {
    control: 'popuplist',
    label: 'align',
    options: ['top', 'middle', 'bottom'],
    defaultValue: 'top',
  },
  gutter: {
    //spacing between grids, could be a number or a object like { xs: 8, sm: 16, md: 24}. or you can use array to make horizontal and vertical spacing work at the same time [horizontal, vertical]
    control: 'number-input',
    label: 'gutter',
    defaultValue: 0,
  },
  justify: {
    control: 'popuplist',
    label: 'justify',
    options: ['start', 'end', 'center', 'space-around', 'space-between'],
    defaultValue: 'center',
  },
}

const Col: PropertyControls = {
  flex: {
    control: 'number-input', // or string
    label: 'flex',
    defaultValue: 1,
  },
  offset: {
    control: 'number-input',
    label: 'offset',
    defaultValue: 0,
  },
  order: {
    control: 'number-input',
    label: 'order',
    defaultValue: 0,
  },
  pull: {
    control: 'number-input',
    label: 'pull',
    defaultValue: 0,
  },
  push: {
    control: 'number-input',
    label: 'push',
    defaultValue: 0,
  },
  span: {
    control: 'number-input',
    label: 'span',
  },
  xs: {
    control: 'number-input', // or an object containing above props
    label: 'xs',
    defaultValue: 0,
  },
  sm: {
    control: 'number-input', // or an object containing above props
    label: 'sm',
    defaultValue: 0,
  },
  md: {
    control: 'number-input', // or an object containing above props
    label: 'md',
    defaultValue: 0,
  },
  lg: {
    control: 'number-input', // or an object containing above props
    label: 'lg',
    defaultValue: 0,
  },
  xl: {
    control: 'number-input', // or an object containing above props
    label: 'xl',
    defaultValue: 0,
  },
  xxl: {
    control: 'number-input', // or an object containing above props
    label: 'xxl',
    defaultValue: 0,
  },
}

const Text: PropertyControls = {
  code: {
    control: 'checkbox',
    label: 'code',
    defaultValue: false,
  },
  copyable: {
    control: 'checkbox', // or { text: string, onCopy: Function }
    label: 'copyable',
    defaultValue: false,
  },
  delete: {
    control: 'checkbox',
    label: 'delete',
    defaultValue: false,
  },
  disabled: {
    control: 'checkbox',
    label: 'disabled',
    defaultValue: false,
  },
  editable: {
    control: 'checkbox', // or { editing: boolean, onStart: Function, onChange: Function(string) }
    label: 'editable',
    defaultValue: false,
  },
  ellipsis: {
    control: 'checkbox',
    label: 'ellipsis',
    defaultValue: false,
  },
  mark: {
    control: 'checkbox',
    label: 'mark',
    defaultValue: false,
  },
  keyboard: {
    control: 'checkbox',
    label: 'keyboard',
    defaultValue: false,
  },
  underline: {
    control: 'checkbox',
    label: 'underline',
    defaultValue: false,
  },
  strong: {
    control: 'checkbox',
    label: 'strong',
    defaultValue: false,
  },
  type: {
    control: 'popuplist',
    label: 'type',
    options: ['secondary', 'warning', 'danger'],
  },
}

const Menu: PropertyControls = {
  defaultOpenKeys: {
    control: 'array',
    propertyControl: {
      control: 'string-input',
    },
    label: 'defaultOpenKeys',
  },
  defaultSelectedKeys: {
    control: 'none',
    label: 'defaultSelectedKeys',
  },
  forceSubMenuRender: {
    control: 'checkbox',
    label: 'forceSubMenuRender',
    defaultValue: false,
  },
  inlineCollapsed: {
    control: 'checkbox',
    label: 'inlineCollapsed',
    defaultValue: false,
  },
  inlineIndent: {
    control: 'number-input',
    label: 'inlineIndent',
    defaultValue: 24,
  },
  mode: {
    control: 'popuplist',
    label: 'mode',
    options: ['vertical', 'horizontal', 'inline'],
    defaultValue: 'inline',
  },
  multiple: {
    control: 'checkbox',
    label: 'multiple',
    defaultValue: false,
  },
  openKeys: {
    control: 'none',
    label: 'openKeys',
  },
  selectable: {
    control: 'checkbox',
    label: 'selectable',
    defaultValue: true,
  },
  selectedKeys: {
    control: 'none',
    label: 'selectedKeys',
  },
  subMenuCloseDelay: {
    control: 'number-input',
    label: 'subMenuCloseDelay',
    defaultValue: 0.1,
  },
  subMenuOpenDelay: {
    control: 'number-input',
    label: 'subMenuOpenDelay',
    defaultValue: 0,
  },
  theme: {
    control: 'popuplist',
    label: 'theme',
    options: ['light', 'dark'],
    defaultValue: 'light',
  },
  onClick: {
    // function({ item, key, keyPath, domEvent })
    control: 'none',
    label: 'onClick',
  },
  onDeselect: {
    // function({ item, key, keyPath, selectedKeys, domEvent })
    control: 'none',
    label: 'onClick',
  },
  onOpenChange: {
    // function(openKeys: string[])
    control: 'none',
    label: 'onClick',
  },
  onSelect: {
    // function({ item, key, keyPath, selectedKeys, domEvent })
    control: 'none',
    label: 'onClick',
  },
  overflowedIndicator: {
    control: 'expression-input',
    label: 'overflowedIndicator',
  },
}

const MenuItem: PropertyControls = {
  disabled: {
    control: 'checkbox',
    label: 'disabled',
    defaultValue: false,
  },
  key: {
    control: 'string-input',
    label: 'key',
  },
  label: {
    control: 'string-input',
    label: 'title',
  },
  icon: {
    control: 'expression-input',
    label: 'icon',
  },
  danger: {
    control: 'checkbox',
    label: 'danger',
    defaultValue: false,
  },
}

const MenuSubMenu: PropertyControls = {
  popupClassName: {
    control: 'string-input',
    label: 'popupClassName',
  },
  disabled: {
    control: 'checkbox',
    label: 'disabled',
    defaultValue: false,
  },
  key: {
    control: 'string-input',
    label: 'key',
  },
  title: {
    control: 'string-input',
    label: 'title',
  },
  icon: {
    control: 'expression-input',
    label: 'icon',
  },
  onTitleClick: {
    // function({ key, domEvent })
    control: 'none',
    label: 'onTitleClick',
  },
}

const MenuItemGroup: PropertyControls = {
  title: {
    control: 'string-input',
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
