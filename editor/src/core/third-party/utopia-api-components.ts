import type {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import {
  defaultFlexRowOrColStyle,
  defaultRectangleElementStyle,
  defaultSceneElementStyle,
  defaultTextElementStyle,
  defaultElementStyle,
} from '../../components/editor/defaults'
import type { JSExpression } from '../shared/element-template'
import { emptyComments, jsxAttributesEntry, jsxElementWithoutUID } from '../shared/element-template'

const BasicUtopiaComponentDescriptor = (
  name: string,
  styleProp: () => JSExpression,
): ComponentDescriptor => {
  return {
    properties: {
      style: {
        control: 'style-controls',
      },
    },
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
  Ellipse: BasicUtopiaComponentDescriptor('Ellipse', defaultElementStyle),
  Rectangle: BasicUtopiaComponentDescriptor('Rectangle', defaultRectangleElementStyle),
  View: BasicUtopiaComponentDescriptor('View', defaultElementStyle),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow', defaultFlexRowOrColStyle),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol', defaultFlexRowOrColStyle),
  Scene: BasicUtopiaComponentDescriptor('Scene', () => defaultSceneElementStyle(null)),
  RemixScene: BasicUtopiaComponentDescriptor('RemixScene', () => defaultSceneElementStyle(null)),
}
