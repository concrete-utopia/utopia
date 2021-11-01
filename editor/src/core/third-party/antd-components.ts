import { importAlias, importDetails, PropertyPathPart } from '../shared/project-file-types'
import {
  componentDescriptor,
  DependencyBoundDescriptors,
  ComponentDescriptor,
} from './third-party-types'
import { jsxElementName, jsxElementWithoutUID } from '../shared/element-template'
import { AntdControls } from '../property-controls/third-party-property-controls/antd-controls'
import { PropertyControls } from 'utopia-api'
import { getDefaultPropsAsAttributes } from './shared'

const StyleObjectProps: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

function createBasicComponent(
  baseVariable: string,
  propertyPathParts: PropertyPathPart[],
  name: string,
  propertyControls: PropertyControls | null,
): ComponentDescriptor {
  const defaultAttributes = getDefaultPropsAsAttributes(propertyControls)
  return componentDescriptor(
    {
      antd: importDetails(null, [importAlias(baseVariable)], null),
      'antd/dist/antd.css': importDetails(null, [], null),
    },
    jsxElementWithoutUID(jsxElementName(baseVariable, propertyPathParts), defaultAttributes, []),
    name,
    { ...StyleObjectProps, ...propertyControls },
  )
}

export const AntdComponents: DependencyBoundDescriptors = {
  '>=4.0.0 <5.0.0': {
    name: 'Antd',
    components: [
      createBasicComponent('DatePicker', [], 'Date Picker', null),
      createBasicComponent('Button', [], 'Button', AntdControls.Button),
      createBasicComponent('InputNumber', [], 'Number Input', null),
      createBasicComponent('Row', [], 'Row', AntdControls.Row),
      createBasicComponent('Col', [], 'Col', AntdControls.Col),
      createBasicComponent('Space', [], 'Space', AntdControls.Space),
      createBasicComponent('Menu', [], 'Menu', AntdControls.Menu),
      createBasicComponent('Menu', ['Item'], 'Menu Item', AntdControls.Item),
      createBasicComponent('Menu', ['SubMenu'], 'Menu SubMenu', AntdControls.SubMenu),
      createBasicComponent('Menu', ['ItemGroup'], 'Menu ItemGroup', AntdControls.ItemGroup),
      createBasicComponent('Typography', ['Text'], 'Typography Text', AntdControls.Text),
    ],
  },
}
