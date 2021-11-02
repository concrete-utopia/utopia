import { PropertyControls } from 'utopia-api'

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
