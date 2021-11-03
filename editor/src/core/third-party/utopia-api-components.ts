import {
  ComponentDescriptor2_RENAME,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'

const BasicUtopiaComponentDescriptor: ComponentDescriptor2_RENAME = {
  propertyControls: {
    style: {
      control: 'style-controls',
    },
  },
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
