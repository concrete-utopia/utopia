import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { AntdComponents } from './antd-components'
import { ReactThreeFiberComponents } from './react-three-fiber-components'
import { RemixRunReactComponents } from './remix-controls'
import { UtopiaApiComponents } from './utopia-api-components'

export const DefaultThirdPartyControlDefinitions: PropertyControlsInfo = {
  '@react-three/fiber': ReactThreeFiberComponents,
  antd: AntdComponents,
  'utopia-api': UtopiaApiComponents,
  '@remix-run/react': RemixRunReactComponents,
}
