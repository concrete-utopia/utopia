import type { PropertyControls } from '../../../components/custom-code/internal-property-controls'

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
  },
  shape: {
    control: 'popuplist',
    label: 'shape',
    options: ['circle', 'round', 'default'],
  },
  icon: {
    control: 'expression-input',
    label: 'icon',
  },
  ghost: {
    control: 'checkbox',
    label: 'ghost',
  },
  block: {
    control: 'checkbox',
    label: 'block',
  },
  danger: {
    control: 'checkbox',
    label: 'danger',
  },
  loading: {
    control: 'checkbox',
    label: 'loading',
  },
  htmlType: {
    control: 'string-input',
    label: 'htmlType',
  },
  target: {
    control: 'string-input',
    label: 'target',
  },
}

const Space: PropertyControls = {
  align: {
    control: 'popuplist',
    label: 'align',
    options: ['start', 'end', 'center', 'baseline'],
  },
  direction: {
    control: 'popuplist',
    label: 'direction',
    options: ['vertical', 'horizontal'],
  },
  size: {
    control: 'popuplist', // or a number
    label: 'size',
    options: ['small', 'middle', 'large'],
  },
}

const Row: PropertyControls = {
  align: {
    control: 'popuplist',
    label: 'align',
    options: ['top', 'middle', 'bottom'],
  },
  gutter: {
    //spacing between grids, could be a number or a object like { xs: 8, sm: 16, md: 24}. or you can use array to make horizontal and vertical spacing work at the same time [horizontal, vertical]
    control: 'number-input',
    label: 'gutter',
  },
  justify: {
    control: 'popuplist',
    label: 'justify',
    options: ['start', 'end', 'center', 'space-around', 'space-between'],
  },
}

const Col: PropertyControls = {
  flex: {
    control: 'number-input', // or string
    label: 'flex',
  },
  offset: {
    control: 'number-input',
    label: 'offset',
  },
  order: {
    control: 'number-input',
    label: 'order',
  },
  pull: {
    control: 'number-input',
    label: 'pull',
  },
  push: {
    control: 'number-input',
    label: 'push',
  },
  span: {
    control: 'number-input',
    label: 'span',
  },
  xs: {
    control: 'number-input', // or an object containing above props
    label: 'xs',
  },
  sm: {
    control: 'number-input', // or an object containing above props
    label: 'sm',
  },
  md: {
    control: 'number-input', // or an object containing above props
    label: 'md',
  },
  lg: {
    control: 'number-input', // or an object containing above props
    label: 'lg',
  },
  xl: {
    control: 'number-input', // or an object containing above props
    label: 'xl',
  },
  xxl: {
    control: 'number-input', // or an object containing above props
    label: 'xxl',
  },
}

const Text: PropertyControls = {
  code: {
    control: 'checkbox',
    label: 'code',
  },
  copyable: {
    control: 'checkbox', // or { text: string, onCopy: Function }
    label: 'copyable',
  },
  delete: {
    control: 'checkbox',
    label: 'delete',
  },
  disabled: {
    control: 'checkbox',
    label: 'disabled',
  },
  editable: {
    control: 'checkbox', // or { editing: boolean, onStart: Function, onChange: Function(string) }
    label: 'editable',
  },
  ellipsis: {
    control: 'checkbox',
    label: 'ellipsis',
  },
  mark: {
    control: 'checkbox',
    label: 'mark',
  },
  keyboard: {
    control: 'checkbox',
    label: 'keyboard',
  },
  underline: {
    control: 'checkbox',
    label: 'underline',
  },
  strong: {
    control: 'checkbox',
    label: 'strong',
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
  },
  inlineCollapsed: {
    control: 'checkbox',
    label: 'inlineCollapsed',
  },
  inlineIndent: {
    control: 'number-input',
    label: 'inlineIndent',
  },
  mode: {
    control: 'popuplist',
    label: 'mode',
    options: ['vertical', 'horizontal', 'inline'],
  },
  multiple: {
    control: 'checkbox',
    label: 'multiple',
  },
  openKeys: {
    control: 'none',
    label: 'openKeys',
  },
  selectable: {
    control: 'checkbox',
    label: 'selectable',
  },
  selectedKeys: {
    control: 'none',
    label: 'selectedKeys',
  },
  subMenuCloseDelay: {
    control: 'number-input',
    label: 'subMenuCloseDelay',
  },
  subMenuOpenDelay: {
    control: 'number-input',
    label: 'subMenuOpenDelay',
  },
  theme: {
    control: 'popuplist',
    label: 'theme',
    options: ['light', 'dark'],
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
  'Typography.Text': Text,
  Menu: Menu,
  'Menu.Item': MenuItem,
  'Menu.SubMenu': MenuSubMenu,
  'Menu.ItemGroup': MenuItemGroup,
}
