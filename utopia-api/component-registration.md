# Registering Components

## High-level overview
- components can be registered using sidecar files
- the registration makes Utopia aware of the components, and makes editing features such as reparenting or insertion work better
- sidecar files can be located anywhere in a Utopia project
- the name of sidecar files has to end with `.utopia.js`

## Example

```typescript
// `utopia-api` offers factory functions such as `Utopia.stringControl()` that
// make creating control descriptions easier
import * as Utopia from 'utopia-api'

// this is the component that will be registered
import { Image } from '@shopify/hydrogen'

const ImageCropControl = Utopia.popupListControl([
  { value: 'top', label: 'top' },
  { value: 'center', label: 'center' },
  { value: 'bottom', label: 'bottom' },
  { value: 'left', label: 'left' },
  { value: 'right', label: 'right' },
])

const ImageTypeControl = Utopia.objectControl({
  altText: Utopia.stringControl(),
  id: Utopia.stringControl(),
  url: Utopia.stringControl(),
  height: Utopia.numberControl(),
  width: Utopia.numberControl(),
})

const Components = {
  // All components from the same module are put under a key that matches the name of the module
  '@shopify/hydrogen': {
    // the key must match the name of the component in the `component` key
    Image: {
      // The name of this component must match the registration key above.
      // This is a safeguard against registering a component that doesn't exist,
      // or not exported from the module it's imported from
      component: Image,

      // The `properties` object makes it possible to set component props through Utopia's inspector.
      // In this object, keys are the names of the properties to register, and values are control
      // descriptions from `utopia-api`
      properties: {
        aspectRatio: Utopia.stringControl(),
        crop: ImageCropControl,
        data: ImageTypeControl,
        srcSetOptions: Utopia.objectControl({
          intervals: Utopia.numberControl(),
          startingWidth: Utopia.numberControl(),
          incrementSize: Utopia.numberControl(),
          placeholderWidth: Utopia.numberControl(),
        }),
      },

      // `supportsChildren` tells Utopia whether this component uses the `children` prop or not.
      // This makes Utopia's Navigator and canvas reparenting features work better with this component
      supportsChildren: false,

      // `variants` defines templates that Utopia can use for insertion.
      variants: [
        {
          // `label` will be used to as a label on the UI
          label: 'Image',

          // `code` will be parsed and checked whether it's syntactically correct, and inserted if this option is chosen
          code:
            "<Image data={{ altText: 'Example image', url: 'https://picsum.photos/200/300', height: 200, width: 300 }} />",
        },
        {
          label: 'Cropped Image',
          code:
            "<Image crop='center' data={{ altText: 'Example image', url: 'https://picsum.photos/200/300', height: 200, width: 300 }} />",
        },
      ],
    },
  },
}

// Utopia reads the component registrations from the default export of this module.
// The module must have a default export. If it's omitted, the component registration
// fails.
export default Components
```

## Property Controls Reference

```tsx
// TODO: list property controls with examples
```
