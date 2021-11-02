import type { PropertyControls } from 'utopia-api'
import { AntdControls } from '../property-controls/third-party-property-controls/antd-controls'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'
import { UtopiaApiControls } from './utopia-api-controls'

export const DefaultThirdPartyControlDefinitions: {
  [packageName: string]: { [componentName: string]: PropertyControls }
} = {
  '@react-three/fiber': ReactThreeFiberControls,
  antd: AntdControls,
  'utopia-api': UtopiaApiControls,
}
