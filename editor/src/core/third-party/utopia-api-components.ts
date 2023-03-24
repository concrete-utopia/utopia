import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import {
  defaultFlexRowOrColStyle,
  defaultRectangleElementStyle,
  defaultSceneElementStyle,
  defaultTextElementStyle,
  defaultViewElementStyle,
} from '../../components/editor/defaults'
import {
  emptyComments,
  JSExpression,
  jsxAttributesEntry,
  jsxElementWithoutUID,
} from '../shared/element-template'

const BasicUtopiaComponentDescriptor = (
  name: string,
  styleProp: JSExpression,
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
        elementToInsert: jsxElementWithoutUID(
          name,
          [jsxAttributesEntry('style', styleProp, emptyComments)],
          [],
        ),
      },
    ],
  }
}

export const UtopiaApiComponents: ComponentDescriptorsForFile = {
  Ellipse: BasicUtopiaComponentDescriptor('Ellipse', defaultViewElementStyle()),
  Rectangle: BasicUtopiaComponentDescriptor('Rectangle', defaultRectangleElementStyle()),
  View: BasicUtopiaComponentDescriptor('View', defaultViewElementStyle()),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow', defaultFlexRowOrColStyle()),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol', defaultFlexRowOrColStyle()),
  Scene: BasicUtopiaComponentDescriptor('Scene', defaultSceneElementStyle(null)),
}
