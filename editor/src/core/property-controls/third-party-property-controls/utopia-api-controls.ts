import type { PropertyControls } from '../../../components/custom-code/internal-property-controls'

const StyleObjectProps: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

export const UtopiaApiControls = {
  Ellipse: StyleObjectProps,
  Rectangle: StyleObjectProps,
  Text: StyleObjectProps,
  View: StyleObjectProps,
  FlexRow: StyleObjectProps,
  FlexCol: StyleObjectProps,
  Scene: StyleObjectProps,
}
