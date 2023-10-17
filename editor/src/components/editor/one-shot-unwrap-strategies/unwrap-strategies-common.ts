import type { ElementPath } from '../../../core/shared/project-file-types'
import type { CustomInspectorStrategy } from '../../inspector/inspector-strategies/inspector-strategy'

export type UnwrapInspectorStrategy = CustomInspectorStrategy<{ newPath: ElementPath }>
