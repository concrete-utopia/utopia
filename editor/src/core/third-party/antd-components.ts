import { AntdControls } from '../property-controls/third-party-property-controls/antd-controls'
import type { PropertyControls } from '../../components/custom-code/internal-property-controls'
import {
  defaultComponentDescriptor,
  type ComponentDescriptor,
  type ComponentDescriptorsForFile,
  ComponentDescriptorDefaults,
} from '../../components/custom-code/code-file'
import type { JSXAttributes } from '../shared/element-template'
import { jsxElementName, jsxElementWithoutUID, simpleAttribute } from '../shared/element-template'
import type { PropertyPathPart } from '../shared/project-file-types'

const StyleObjectProps: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

function createBasicComponent(
  baseVariable: string,
  propertyPathParts: PropertyPathPart[],
  propertyControls: PropertyControls,
  attributes?: () => JSXAttributes,
): ComponentDescriptor {
  return {
    properties: { ...StyleObjectProps, ...propertyControls },
    supportsChildren: false,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: [baseVariable, ...propertyPathParts].join('.'),
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
        elementToInsert: () =>
          jsxElementWithoutUID(
            jsxElementName(baseVariable, propertyPathParts),
            attributes?.() ?? [],
            [],
          ),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
    inspector: 'all',
  }
}

export const AntdComponents: ComponentDescriptorsForFile = {
  DatePicker: createBasicComponent('DatePicker', [], {}),
  Button: createBasicComponent('Button', [], AntdControls.Button, () => [
    simpleAttribute('disabled', false),
    simpleAttribute('type', 'default'),
    simpleAttribute('shape', 'default'),
    simpleAttribute('ghost', false),
    simpleAttribute('block', false),
    simpleAttribute('danger', false),
    simpleAttribute('loading', false),
    simpleAttribute('target', 'button'),
  ]),
  InputNumber: createBasicComponent('InputNumber', [], {}),
  Row: createBasicComponent('Row', [], AntdControls.Row, () => [
    simpleAttribute('align', 'top'),
    simpleAttribute('gutter', 0),
    simpleAttribute('justify', 'center'),
  ]),
  Col: createBasicComponent('Col', [], AntdControls.Col, () => [
    simpleAttribute('flex', 1),
    simpleAttribute('offset', 0),
    simpleAttribute('order', 0),
    simpleAttribute('pull', 0),
    simpleAttribute('push', 0),
    simpleAttribute('xs', 0),
    simpleAttribute('sm', 0),
    simpleAttribute('md', 0),
    simpleAttribute('lg', 0),
    simpleAttribute('xl', 0),
    simpleAttribute('xxl', 0),
  ]),
  Space: createBasicComponent('Space', [], AntdControls.Space, () => [
    simpleAttribute('align', 'center'),
    simpleAttribute('direction', 'vertical'),
    simpleAttribute('size', 'middle'),
  ]),
  Menu: createBasicComponent('Menu', [], AntdControls.Menu, () => [
    simpleAttribute('forceSubMenuRender', false),
    simpleAttribute('inlineCollapsed', false),
    simpleAttribute('inlineIndent', 24),
    simpleAttribute('mode', 'inline'),
    simpleAttribute('multiple', false),
    simpleAttribute('selectable', true),
    simpleAttribute('subMenuCloseDelay', 0.1),
    simpleAttribute('subMenuOpenDelay', 0.0),
    simpleAttribute('theme', 'light'),
  ]),
  'Menu.Item': createBasicComponent('Menu', ['Item'], AntdControls['Menu.Item'], () => [
    simpleAttribute('disabled', false),
    simpleAttribute('danger', false),
  ]),
  'Menu.SubMenu': createBasicComponent('Menu', ['SubMenu'], AntdControls['Menu.SubMenu'], () => [
    simpleAttribute('disabled', false),
  ]),
  'Menu.ItemGroup': createBasicComponent('Menu', ['ItemGroup'], AntdControls['Menu.ItemGroup']),
  'Typography.Text': createBasicComponent(
    'Typography',
    ['Text'],
    AntdControls['Typography.Text'],
    () => [
      simpleAttribute('code', false),
      simpleAttribute('copyable', false),
      simpleAttribute('delete', false),
      simpleAttribute('disabled', false),
      simpleAttribute('editable', false),
      simpleAttribute('ellipsis', false),
      simpleAttribute('mark', false),
      simpleAttribute('keyboard', false),
      simpleAttribute('underline', false),
      simpleAttribute('strong', false),
    ],
  ),
}
