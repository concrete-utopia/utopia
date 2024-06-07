import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { RemixRunReactComponents } from './remix-controls'
import { UtopiaApiComponents } from './utopia-api-components'

export const DefaultThirdPartyControlDefinitions: PropertyControlsInfo = {
  'utopia-api': UtopiaApiComponents,
  '@remix-run/react': RemixRunReactComponents,
}
