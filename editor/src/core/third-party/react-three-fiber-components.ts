import { PropertyControls } from 'utopia-api'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'
import {
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'
import { parsePropertyControls } from '../property-controls/property-controls-parser'

function createBasicComponent(propertyControls: PropertyControls): ComponentDescriptor {
  return {
    propertyControls: parsePropertyControls(propertyControls),
    componentInfo: {
      requiredImports: [],
    },
  }
}

export const ReactThreeFiberComponents: ComponentDescriptorsForFile = {
  color: createBasicComponent(ReactThreeFiberControls.color),
  fog: createBasicComponent(ReactThreeFiberControls.fog),
  ambientLight: createBasicComponent(ReactThreeFiberControls.ambientLight),
  directionalLight: createBasicComponent(ReactThreeFiberControls.directionalLight),
  pointLight: createBasicComponent(ReactThreeFiberControls.pointLight),
  spotLight: createBasicComponent(ReactThreeFiberControls.spotLight),
  boxGeometry: createBasicComponent(ReactThreeFiberControls.boxGeometry),
  planeGeometry: createBasicComponent(ReactThreeFiberControls.planeGeometry),
  sphereGeometry: createBasicComponent(ReactThreeFiberControls.sphereGeometry),
  meshBasicMaterial: createBasicComponent(ReactThreeFiberControls.meshBasicMaterial),
  meshStandardMaterial: createBasicComponent(ReactThreeFiberControls.meshStandardMaterial),
  shadowMaterial: createBasicComponent(ReactThreeFiberControls.shadowMaterial),
}
