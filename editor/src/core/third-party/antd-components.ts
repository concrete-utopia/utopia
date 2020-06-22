import { importAlias, importDetails } from '../shared/project-file-types'
import {
  componentDescriptor,
  DependencyBoundDescriptors,
  ComponentDescriptor,
} from './third-party-types'
import { jsxElement, jsxElementName } from '../shared/element-template'

function createBasicComponent(element: string, name: string): ComponentDescriptor {
  return componentDescriptor(
    { antd: importDetails(null, [importAlias(element)], null) },
    jsxElement(jsxElementName(element, []), {}, [], null),
    name,
  )
}

export const AntdComponents: DependencyBoundDescriptors = {
  '>=4.0.0 <5.0.0': {
    name: 'Antd',
    components: [
      createBasicComponent('DatePicker', 'Date Picker'),
      createBasicComponent('Button', 'Button'),
      createBasicComponent('InputNumber', 'Number Input'),
    ],
  },
}
