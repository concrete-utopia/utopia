import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import { parsePropertyControls } from '../property-controls/property-controls-parser'
import { jsxElementWithoutUID } from '../shared/element-template'

const BasicUtopiaComponentDescriptor = (name: string): ComponentDescriptor => {
  return {
    propertyControls: parsePropertyControls({
      style: {
        control: 'style-controls',
      },
    }),
    insertOptions: [
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
        elementToInsert: jsxElementWithoutUID(name, [], []),
      },
    ],
  }
}

export const UtopiaApiComponents: ComponentDescriptorsForFile = {
  Ellipse: BasicUtopiaComponentDescriptor('Ellipse'),
  Rectangle: BasicUtopiaComponentDescriptor('Rectangle'),
  Text: BasicUtopiaComponentDescriptor('Text'),
  View: BasicUtopiaComponentDescriptor('View'),
  FlexRow: BasicUtopiaComponentDescriptor('FlexRow'),
  FlexCol: BasicUtopiaComponentDescriptor('FlexCol'),
  Scene: BasicUtopiaComponentDescriptor('Scene'),
}
