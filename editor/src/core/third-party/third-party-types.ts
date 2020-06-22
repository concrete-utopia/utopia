import { JSXElement } from '../shared/element-template'
import { Imports } from '../shared/project-file-types'

export interface ComponentDescriptor {
  importsToAdd: Imports
  element: JSXElement
  name: string
}

export function componentDescriptor(
  importsToAdd: Imports,
  element: JSXElement,
  name: string,
): ComponentDescriptor {
  return {
    importsToAdd: importsToAdd,
    element: element,
    name: name,
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
