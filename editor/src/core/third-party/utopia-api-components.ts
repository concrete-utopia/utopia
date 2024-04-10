import {
  defaultComponentDescriptor,
  type ComponentDescriptor,
  type ComponentDescriptorsForFile,
  type Children,
  ComponentDescriptorDefaults,
} from '../../components/custom-code/code-file'
import {
  defaultFlexRowOrColStyle,
  defaultRectangleElementStyle,
  defaultSceneElementStyle,
  defaultElementStyle,
} from '../../components/editor/defaults'
import type { JSExpression } from '../shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  jsxAttributesEntry,
  jsxElementWithoutUID,
} from '../shared/element-template'

const BasicUtopiaComponentDescriptor = (
  name: string,
  children: Children,
  styleProp: () => JSExpression,
): ComponentDescriptor => {
  return {
    properties: {
      style: {
        control: 'style-controls',
      },
    },
    children: children,
    variants: [
      {
        insertMenuLabel: name,
        importsToAdd: {
          'utopia-api': {
            importedWithName: null,
            importedFromWithin: [
              {
                name: name,
                alias: name,
              },
            ],
            importedAs: null,
          },
        },
        elementToInsert: () =>
          jsxElementWithoutUID(name, [jsxAttributesEntry('style', styleProp(), emptyComments)], []),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  }
}

const BasicUtopiaSceneDescriptor = (
  name: string,
  children: Children,
  styleProp: () => JSExpression,
): ComponentDescriptor => {
  return {
    properties: {
      style: {
        control: 'style-controls',
      },
    },
    children: children,
    variants: [
      {
        insertMenuLabel: name,
        importsToAdd: {
          'utopia-api': {
            importedWithName: null,
            importedFromWithin: [
              {
                name: name,
                alias: name,
              },
            ],
            importedAs: null,
          },
        },
        elementToInsert: () =>
          jsxElementWithoutUID(
            name,
            [
              jsxAttributesEntry(
                'commentId',
                jsExpressionValue('scene', emptyComments),
                emptyComments,
              ),
              jsxAttributesEntry('style', styleProp(), emptyComments),
            ],
            [],
          ),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  }
}

export const UtopiaApiComponents: ComponentDescriptorsForFile = {
  Ellipse: BasicUtopiaComponentDescriptor('Ellipse', 'not-supported', defaultElementStyle),
  Rectangle: BasicUtopiaComponentDescriptor(
    'Rectangle',
    'not-supported',
    defaultRectangleElementStyle,
  ),
  View: BasicUtopiaComponentDescriptor('View', 'supported', defaultElementStyle),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow', 'supported', defaultFlexRowOrColStyle),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol', 'supported', defaultFlexRowOrColStyle),
  Scene: BasicUtopiaSceneDescriptor('Scene', 'supported', () => defaultSceneElementStyle(null)),
  RemixScene: BasicUtopiaSceneDescriptor('RemixScene', 'not-supported', () =>
    defaultSceneElementStyle(null),
  ),
}
