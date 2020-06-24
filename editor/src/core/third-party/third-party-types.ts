import { JSXElement } from '../shared/element-template'
import { Imports } from '../shared/project-file-types'
import { PropertyControls } from 'utopia-api'

export interface ComponentDescriptor {
  importsToAdd: Imports
  element: JSXElement
  name: string
  propertyControls: PropertyControls | null
}

export function componentDescriptor(
  importsToAdd: Imports,
  element: JSXElement,
  name: string,
  propertyControls: PropertyControls | null,
): ComponentDescriptor {
  return {
    importsToAdd: importsToAdd,
    element: element,
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
