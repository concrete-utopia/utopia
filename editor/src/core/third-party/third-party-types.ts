import { ImportType, PropertyControls } from 'utopia-api'

export interface ComponentDescriptor {
  importsToAdd: Array<ImportType>
  name: string
  propertyControls: PropertyControls
}

export function componentDescriptor(
  importsToAdd: Array<ImportType>,
  name: string,
  propertyControls: PropertyControls,
): ComponentDescriptor {
  return {
    importsToAdd: importsToAdd,
    name: name,
    propertyControls: propertyControls,
  }
}
