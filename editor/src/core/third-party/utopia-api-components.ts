import type {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import {
  defaultFlexRowOrColStyle,
  defaultRectangleElementStyle,
  defaultSceneElementStyle,
  defaultElementStyle,
} from '../../components/editor/defaults'
import type { JSExpression } from '../shared/element-template'
import { emptyComments, jsxAttributesEntry, jsxElementWithoutUID } from '../shared/element-template'

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
  }
}

export const UtopiaApiComponents: ComponentDescriptorsForFile = {
  Ellipse: BasicUtopiaComponentDescriptor('Ellipse', false, defaultElementStyle),
  Rectangle: BasicUtopiaComponentDescriptor('Rectangle', false, defaultRectangleElementStyle),
  View: BasicUtopiaComponentDescriptor('View', true, defaultElementStyle),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow', true, defaultFlexRowOrColStyle),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol', true, defaultFlexRowOrColStyle),
  Scene: BasicUtopiaComponentDescriptor('Scene', true, () => defaultSceneElementStyle(null)),
  RemixScene: BasicUtopiaComponentDescriptor('RemixScene', false, () =>
    defaultSceneElementStyle(null),
  ),
}
