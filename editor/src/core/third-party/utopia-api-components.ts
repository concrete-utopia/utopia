import {
  defaultComponentDescriptor,
  type ComponentDescriptor,
  type ComponentDescriptorsForFile,
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

const IntrinsicComponentDescriptor = (
  name: string,
  insertMenuLabel: string,
  supportsChildren: boolean,
  styleProp: () => JSExpression,
): ComponentDescriptor => {
  return {
    properties: {
      style: {
        control: 'style-controls',
      },
    },
    supportsChildren: supportsChildren,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: insertMenuLabel,
        importsToAdd: {},
        elementToInsert: () =>
          jsxElementWithoutUID(
            name,
            [
              jsxAttributesEntry('style', styleProp(), emptyComments),
              jsxAttributesEntry(
                'data-label',
                jsExpressionValue('Rectangle', emptyComments),
                emptyComments,
              ),
            ],
            [],
          ),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  }
}
const BasicUtopiaComponentDescriptor = (
  name: string,
  supportsChildren: boolean,
  styleProp: () => JSExpression,
): ComponentDescriptor => {
  return {
    properties: {
      style: {
        control: 'style-controls',
      },
    },
    supportsChildren: supportsChildren,
    preferredChildComponents: [],
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
  supportsChildren: boolean,
  styleProp: () => JSExpression,
): ComponentDescriptor => {
  return {
    properties: {
      style: {
        control: 'style-controls',
      },
    },
    supportsChildren: supportsChildren,
    preferredChildComponents: [],
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
    icon: 'scene',
  }
}

export const UtopiaApiComponents: ComponentDescriptorsForFile = {
  Ellipse: BasicUtopiaComponentDescriptor('Ellipse', false, defaultElementStyle),
  Rectangle: IntrinsicComponentDescriptor('div', 'Rectangle', false, defaultRectangleElementStyle),
  View: BasicUtopiaComponentDescriptor('View', true, defaultElementStyle),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow', true, defaultFlexRowOrColStyle),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol', true, defaultFlexRowOrColStyle),
  Scene: BasicUtopiaSceneDescriptor('Scene', true, () => defaultSceneElementStyle(null)),
  RemixScene: BasicUtopiaSceneDescriptor('RemixScene', false, () => defaultSceneElementStyle(null)),
}
