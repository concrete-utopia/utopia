import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { AntdControls } from '../property-controls/third-party-property-controls/antd-controls'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'
import { UtopiaApiControls } from '../property-controls/third-party-property-controls/utopia-api-controls'

export const DefaultThirdPartyControlDefinitions: PropertyControlsInfo = {
  '@react-three/fiber': ReactThreeFiberControls,
  antd: AntdControls,
  'utopia-api': UtopiaApiControls,
}
