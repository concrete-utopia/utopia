import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import { parsePropertyControls } from '../property-controls/property-controls-parser'

const BasicUtopiaComponentDescriptor: ComponentDescriptor = {
  propertyControls: parsePropertyControls({
    style: {
      control: 'style-controls',
    },
  }),
  componentInfo: {},
}

export const UtopiaApiComponents: ComponentDescriptorsForFile = {
  Ellipse: BasicUtopiaComponentDescriptor,
  Rectangle: BasicUtopiaComponentDescriptor,
  Text: BasicUtopiaComponentDescriptor,
  View: BasicUtopiaComponentDescriptor,
  FlexRow: BasicUtopiaComponentDescriptor,
  FlexCol: BasicUtopiaComponentDescriptor,
  Scene: BasicUtopiaComponentDescriptor,
}
