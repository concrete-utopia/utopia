import type { ProjectContentTreeRoot } from '../../components/assets'
import { getProjectFileByFilePath } from '../../components/assets'
import { objectKeyParser, parseString } from '../../utils/value-parser-utils'
import { defaultEither, eitherToMaybe, isLeft } from './either'
import { is } from './equality-utils'
import { memoize } from './memoize'
import type { ProjectFile } from './project-file-types'
import { isTextFile, ProjectContents } from './project-file-types'

function parsePackageJsonInner(
  packageJson: ProjectFile | undefined | null,
): Record<string, any> | null {
  if (packageJson != null && isTextFile(packageJson)) {
    try {
      return JSON.parse(packageJson.fileContents.code)
    } catch (e) {
      console.error('Invalid package.json contents.', e)
      return null
    }
  } else {
    return null
  }
}

function packageJsonEquals(l: ProjectFile | undefined, r: ProjectFile | undefined): boolean {
  if (is(l, r)) {
    return true
  } else {
    return (
      l != undefined &&
      isTextFile(l) &&
      r != undefined &&
      isTextFile(r) &&
      l.fileContents === r.fileContents
    )
  }
}

const parsePackageJson = memoize(parsePackageJsonInner, {
  maxSize: 1,
  matchesArg: packageJsonEquals,
})

function getParsedPackageJson(projectContents: ProjectContentTreeRoot): Record<string, any> | null {
  return parsePackageJson(getProjectFileByFilePath(projectContents, '/package.json'))
}

function getUtopiaSpecificStringSetting(
  prop: string,
  projectContents: ProjectContentTreeRoot,
): string | null {
  const packageJson = getParsedPackageJson(projectContents)
  const parseField = objectKeyParser(objectKeyParser(parseString, prop), 'utopia')
  const parseResult = parseField(packageJson)
  return eitherToMaybe(parseResult)
}

export function getMainUIFilename(projectContents: ProjectContentTreeRoot): string | null {
  return getUtopiaSpecificStringSetting('main-ui', projectContents)
}

export function getMainHTMLFilename(projectContents: ProjectContentTreeRoot): string {
  const setValue = getUtopiaSpecificStringSetting('html', projectContents)
  return setValue ?? 'public/index.html'
}

export function getMainJSFilename(projectContents: ProjectContentTreeRoot): string {
  const setValue = getUtopiaSpecificStringSetting('js', projectContents)
  return setValue ?? 'src/index.js'
}
