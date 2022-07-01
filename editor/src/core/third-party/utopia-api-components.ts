import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import {
  defaultFlexRowOrColStyle,
  defaultSceneElementStyle,
  defaultTextElementStyle,
  defaultViewElementStyle,
} from '../../components/editor/defaults'
import {
  emptyComments,
  JSXAttribute,
  jsxAttributesEntry,
  jsxElementWithoutUID,
} from '../shared/element-template'

const BasicUtopiaComponentDescriptor = (
  name: string,
  styleProp: JSXAttribute,
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
  Rectangle: BasicUtopiaComponentDescriptor('Rectangle', defaultViewElementStyle()),
  Text: BasicUtopiaComponentDescriptor('Text', defaultTextElementStyle()),
  View: BasicUtopiaComponentDescriptor('View', defaultViewElementStyle()),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow', defaultFlexRowOrColStyle()),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol', defaultFlexRowOrColStyle()),
  Scene: BasicUtopiaComponentDescriptor('Scene', defaultSceneElementStyle(null)),
}
