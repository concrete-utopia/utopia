import { JSXElementWithoutUID } from '../shared/element-template'
import { Imports } from '../shared/project-file-types'
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

export interface DependencyDescriptor {
  name: string
  components: Array<ComponentDescriptor>
}

// A dictionary of bounds to the descriptors for those respective versions.
export type DependencyBoundDescriptors = { [dependencyBounds: string]: DependencyDescriptor }

// A dictionary of dependency names to the various instances of that dependency.
export type DependenciesDescriptors = { [dependencyName: string]: DependencyBoundDescriptors }
