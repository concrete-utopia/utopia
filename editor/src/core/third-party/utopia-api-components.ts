import { importAlias, importDetails } from '../shared/project-file-types'
import {
  componentDescriptor,
  DependencyBoundDescriptors,
  ComponentDescriptor,
} from './third-party-types'
import {
  JSXElementChildren,
  jsxElementName,
  jsxElementWithoutUID,
  jsxTextBlock,
} from '../shared/element-template'
import { PropertyControls } from 'utopia-api'
import { getDefaultPropsAsAttributes } from './shared'

function createBasicUtopiaComponent(
  baseVariable: string,
  name: string,
  propertyControls: PropertyControls | null,
  children: JSXElementChildren = [],
): ComponentDescriptor {
  const defaultAttributes = getDefaultPropsAsAttributes(propertyControls)
  return componentDescriptor(
    {
      'utopia-api': importDetails(null, [importAlias(baseVariable)], null),
    },
    jsxElementWithoutUID(jsxElementName(baseVariable, []), defaultAttributes, children),
    name,
    propertyControls,
  )
}

const StyleObjectProps: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

export const UtopiaApiComponents: DependencyBoundDescriptors = {
  '>=0.0.0 <1.0.0': {
    name: 'utopia-api',
    components: [
      createBasicUtopiaComponent('Ellipse', 'Ellipse', StyleObjectProps),
      createBasicUtopiaComponent('Rectangle', 'Rectangle', StyleObjectProps),
      createBasicUtopiaComponent('Text', 'Text', StyleObjectProps, [jsxTextBlock('Utopia')]),
      createBasicUtopiaComponent('View', 'View', StyleObjectProps),
      createBasicUtopiaComponent('FlexRow', 'FlexRow', StyleObjectProps),
      createBasicUtopiaComponent('FlexCol', 'FlexCol', StyleObjectProps),
      createBasicUtopiaComponent('Scene', 'Scene', StyleObjectProps),
    ],
  },
}
