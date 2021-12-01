import { PropertyControls } from 'utopia-api'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'
import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import { parsePropertyControls } from '../property-controls/property-controls-parser'
import { jsxElementWithoutUID } from '../shared/element-template'

function createBasicComponent(
  name: string,
  propertyControls: PropertyControls,
): ComponentDescriptor {
  return {
    propertyControls: parsePropertyControls(propertyControls),
    insertOptions: [
      {
        insertMenuLabel: name,
        importsToAdd: {},
        elementToInsert: jsxElementWithoutUID(name, [], []),
      },
    ],
  }
}

export const ReactThreeFiberComponents: ComponentDescriptorsForFile = {
  color: createBasicComponent('color', ReactThreeFiberControls.color),
  fog: createBasicComponent('fog', ReactThreeFiberControls.fog),
  ambientLight: createBasicComponent('ambientLight', ReactThreeFiberControls.ambientLight),
  directionalLight: createBasicComponent(
    'directionalLight',
    ReactThreeFiberControls.directionalLight,
  ),
  pointLight: createBasicComponent('pointLight', ReactThreeFiberControls.pointLight),
  spotLight: createBasicComponent('spotLight', ReactThreeFiberControls.spotLight),
  boxGeometry: createBasicComponent('boxGeometry', ReactThreeFiberControls.boxGeometry),
  planeGeometry: createBasicComponent('planeGeometry', ReactThreeFiberControls.planeGeometry),
  sphereGeometry: createBasicComponent('sphereGeometry', ReactThreeFiberControls.sphereGeometry),
  meshBasicMaterial: createBasicComponent(
    'meshBasicMaterial',
    ReactThreeFiberControls.meshBasicMaterial,
  ),
  meshStandardMaterial: createBasicComponent(
    'meshStandardMaterial',
    ReactThreeFiberControls.meshStandardMaterial,
  ),
  shadowMaterial: createBasicComponent('shadowMaterial', ReactThreeFiberControls.shadowMaterial),
}
