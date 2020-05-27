import { PropertyPath } from '../../core/shared/project-file-types'

export interface InspectorPartProps<T> {
  input: T
  onSubmitValue: (output: T, paths: Array<PropertyPath>) => void
}
