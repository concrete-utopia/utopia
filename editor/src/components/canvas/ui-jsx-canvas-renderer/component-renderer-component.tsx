import type { PropertyControls } from '../../custom-code/internal-property-controls'
import type {
  UTOPIA_INSTANCE_PATH,
  UTOPIA_PATH_KEY,
  UTOPIA_STATIC_PATH_KEY,
} from '../../../core/model/utopia-constants'
import type { ElementPath } from '../../../core/shared/project-file-types'

export type ComponentRendererComponent = React.ComponentType<
  React.PropsWithChildren<{
    [UTOPIA_INSTANCE_PATH]: ElementPath
    [UTOPIA_PATH_KEY]?: string
    [UTOPIA_STATIC_PATH_KEY]?: string
  }>
> & {
  topLevelElementName: string | null
  propertyControls?: PropertyControls
  utopiaType: 'UTOPIA_COMPONENT_RENDERER_COMPONENT'
  filePath: string
  originalName: string | null
}

export function isComponentRendererComponent(
  component:
    | ComponentRendererComponent
    | React.ComponentType<React.PropsWithChildren<unknown>>
    | null
    | undefined,
): component is ComponentRendererComponent {
  return (
    component != null &&
    typeof component === 'function' &&
    (component as ComponentRendererComponent).utopiaType === 'UTOPIA_COMPONENT_RENDERER_COMPONENT'
  )
}
