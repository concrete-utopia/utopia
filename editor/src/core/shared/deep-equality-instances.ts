import {
  arrayDeepEquality,
  createCallFromEqualsFunction,
  KeepDeepEqualityCall,
} from './deep-equality'
import { TemplatePath } from './project-file-types'
import * as TP from './template-path'

export const TemplatePathKeepDeepEquality: KeepDeepEqualityCall<TemplatePath> = createCallFromEqualsFunction(
  (first: TemplatePath, second: TemplatePath) => {
    return TP.pathsEqual(first, second)
  },
)

export const TemplatePathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  TemplatePath
>> = arrayDeepEquality(TemplatePathKeepDeepEquality)
