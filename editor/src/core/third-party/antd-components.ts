import { AntdControls } from '../property-controls/third-party-property-controls/antd-controls'
import { PropertyControls } from 'utopia-api'
import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import { parsePropertyControls } from '../property-controls/property-controls-parser'
import { jsxElementWithoutUID } from '../shared/element-template'

const StyleObjectProps: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

function createBasicComponent(
  baseVariable: string,
  propertyControls: PropertyControls,
): ComponentDescriptor {
  return {
    propertyControls: parsePropertyControls({ ...StyleObjectProps, ...propertyControls }),
    componentInfo: {
      importsToAdd: {
        antd: {
          importedWithName: null,
          importedFromWithin: [
            {
              name: baseVariable,
              alias: baseVariable,
            },
          ],
          importedAs: null,
        },
        'antd/dist/antd.css': {
          importedWithName: null,
          importedFromWithin: [],
          importedAs: null,
        },
      },
      elementToInsert: jsxElementWithoutUID(baseVariable, [], []),
    },
  }
}

export const AntdComponents: ComponentDescriptorsForFile = {
  DatePicker: createBasicComponent('DatePicker', {}),
  Button: createBasicComponent('Button', AntdControls.Button),
  InputNumber: createBasicComponent('InputNumber', {}),
  Row: createBasicComponent('Row', AntdControls.Row),
  Col: createBasicComponent('Col', AntdControls.Col),
  Space: createBasicComponent('Space', AntdControls.Space),
  Menu: createBasicComponent('Menu', AntdControls.Menu),
  'Menu.Item': createBasicComponent('Menu', AntdControls['Menu.Item']),
  'Menu.SubMenu': createBasicComponent('Menu', AntdControls['Menu.SubMenu']),
  'Menu.ItemGroup': createBasicComponent('Menu', AntdControls['Menu.ItemGroup']),
  'Typography.Text': createBasicComponent('Typography', AntdControls['Typography.Text']),
}
