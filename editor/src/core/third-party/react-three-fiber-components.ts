import { importAlias, importDetails, PropertyPathPart } from '../shared/project-file-types'
import {
  componentDescriptor,
  DependencyBoundDescriptors,
  ComponentDescriptor,
} from './third-party-types'
import { jsxElementName, jsxElementWithoutUID } from '../shared/element-template'
import { PropertyControls } from 'utopia-api'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'

function createBasicComponent(
  baseVariable: string,
  propertyControls: PropertyControls | null,
): ComponentDescriptor {
  return componentDescriptor(
    [],
    jsxElementWithoutUID(
      jsxElementName(baseVariable, []),
      [], // Note: we don't have default props for react-three-fiber insertion
      [],
    ),
    baseVariable,
    { ...propertyControls },
  )
}

export const ReactThreeFiberComponents: DependencyBoundDescriptors = {
  '*': {
    name: '@react-three/fiber',
    components: [
      createBasicComponent('color', ReactThreeFiberControls.color),
      createBasicComponent('fog', ReactThreeFiberControls.fog),
      createBasicComponent('ambientLight', ReactThreeFiberControls.ambientLight),
      createBasicComponent('directionalLight', ReactThreeFiberControls.directionalLight),
      createBasicComponent('pointLight', ReactThreeFiberControls.pointLight),
      createBasicComponent('spotLight', ReactThreeFiberControls.spotLight),
      createBasicComponent('boxGeometry', ReactThreeFiberControls.boxGeometry),
      createBasicComponent('planeGeometry', ReactThreeFiberControls.planeGeometry),
      createBasicComponent('sphereGeometry', ReactThreeFiberControls.sphereGeometry),
      createBasicComponent('meshBasicMaterial', ReactThreeFiberControls.meshBasicMaterial),
      createBasicComponent('meshStandardMaterial', ReactThreeFiberControls.meshStandardMaterial),
      createBasicComponent('shadowMaterial', ReactThreeFiberControls.shadowMaterial),
    ],
  },
}
