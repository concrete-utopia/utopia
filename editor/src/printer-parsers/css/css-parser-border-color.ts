import type { Either } from '../../core/shared/either'
import { isRight, left } from '../../core/shared/either'
import type { CSSColor } from '../../components/inspector/common/css-utils'
import { parseLexedColor } from './css-parser-utils'

export function parseBorderColor(value: unknown): Either<string, CSSColor> {
  const parsed = parseLexedColor(value)
  if (isRight(parsed)) {
    return parsed
  } else {
    return left(parsed.value.type)
  }
}
