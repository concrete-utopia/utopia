import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../components/assets'
import { Either, left, right } from '../shared/either'
import { RequireFn } from '../shared/npm-dependency-types'
import { isTextFile } from '../shared/project-file-types'

export const TailwindConfigPath = '/tailwind.config.js'

export function hasTailwindConfig(projectContents: ProjectContentTreeRoot): boolean {
  const possibleFile = getContentsTreeFileFromString(projectContents, TailwindConfigPath)
  return possibleFile != null && isTextFile(possibleFile)
}

export function getTailwindConfig(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
): Either<any, unknown> {
  if (hasTailwindConfig(projectContents)) {
    try {
      const requireResult = requireFn('/', TailwindConfigPath)
      return right(requireResult)
    } catch (error) {
      return left(error)
    }
  } else {
    return left('No valid tailwind config available.')
  }
}
