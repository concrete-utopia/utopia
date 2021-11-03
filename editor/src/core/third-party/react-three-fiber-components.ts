import { PropertyControls } from 'utopia-api'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'
import {
  ComponentDescriptor2_RENAME,
  ComponentDescriptorsForFile,
} from '../../components/custom-code/code-file'

function createBasicComponent(propertyControls: PropertyControls): ComponentDescriptor2_RENAME {
  return {
    propertyControls: propertyControls,
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
