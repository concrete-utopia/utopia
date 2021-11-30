import { AntdControls } from '../property-controls/third-party-property-controls/antd-controls'
import { PropertyControls } from 'utopia-api'
import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import { parsePropertyControls } from '../property-controls/property-controls-parser'
import { jsxElementName, jsxElementWithoutUID } from '../shared/element-template'
import { getDefaultPropsAsAttributesFromParsedControls } from './shared'
import { PropertyPathPart } from '../shared/project-file-types'

const StyleObjectProps: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

function createBasicComponent(
  baseVariable: string,
  propertyPathParts: PropertyPathPart[],
  propertyControls: PropertyControls,
): ComponentDescriptor {
  const parsedControls = parsePropertyControls(propertyControls)
  const defaultAttributes = getDefaultPropsAsAttributesFromParsedControls(parsedControls)
  return {
    propertyControls: parsePropertyControls({ ...StyleObjectProps, ...propertyControls }),
    insertOptions: [
      {
        insertMenuLabel: baseVariable,
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
        elementToInsert: jsxElementWithoutUID(
          jsxElementName(baseVariable, propertyPathParts),
          defaultAttributes,
          [],
        ),
      },
    ],
  }
}

export const AntdComponents: ComponentDescriptorsForFile = {
  DatePicker: createBasicComponent('DatePicker', [], {}),
  Button: createBasicComponent('Button', [], AntdControls.Button),
  InputNumber: createBasicComponent('InputNumber', [], {}),
  Row: createBasicComponent('Row', [], AntdControls.Row),
  Col: createBasicComponent('Col', [], AntdControls.Col),
  Space: createBasicComponent('Space', [], AntdControls.Space),
  Menu: createBasicComponent('Menu', [], AntdControls.Menu),
  'Menu.Item': createBasicComponent('Menu', ['Item'], AntdControls['Menu.Item']),
  'Menu.SubMenu': createBasicComponent('Menu', ['SubMenu'], AntdControls['Menu.SubMenu']),
  'Menu.ItemGroup': createBasicComponent('Menu', ['ItemGroup'], AntdControls['Menu.ItemGroup']),
  'Typography.Text': createBasicComponent('Typography', ['Text'], AntdControls['Typography.Text']),
}
