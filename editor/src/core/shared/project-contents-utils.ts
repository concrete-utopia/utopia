import { objectKeyParser, parseString } from '../../utils/value-parser-utils'
import { defaultEither, eitherToMaybe, isLeft } from './either'
import { is } from './equality-utils'
import { memoize } from './memoize'
import { isCodeFile, ProjectContents, ProjectFile } from './project-file-types'

function parsePackageJsonInner(packageJson: ProjectFile | null): Record<string, any> | null {
  if (packageJson != null && isCodeFile(packageJson)) {
    try {
      return JSON.parse(packageJson.fileContents)
    } catch (e) {
      console.error('Invalid package.json contents.', e)
      return null
    }
  } else {
    return null
  }
}

function packageJsonEquals(l: ProjectFile | null, r: ProjectFile | null): boolean {
  if (is(l, r)) {
    return true
  } else {
    return (
      l != null && isCodeFile(l) && r != null && isCodeFile(r) && l.fileContents === r.fileContents
    )
  }
}

const parsePackageJson = memoize(parsePackageJsonInner, { maxSize: 1, equals: packageJsonEquals })

function getParsedPackageJson(projectContents: ProjectContents): Record<string, any> | null {
  return parsePackageJson(projectContents['/package.json'])
}

function getUtopiaSpecificStringSetting(
  prop: string,
  projectContents: ProjectContents,
): string | null {
  const packageJson = getParsedPackageJson(projectContents)
  const parseField = objectKeyParser(objectKeyParser(parseString, prop), 'utopia')
  const parseResult = parseField(packageJson)
  return eitherToMaybe(parseResult)
}

export function getMainUIFilename(projectContents: ProjectContents): string | null {
  return getUtopiaSpecificStringSetting('main-ui', projectContents)
}

export function getMainHTMLFilename(projectContents: ProjectContents): string {
  const setValue = getUtopiaSpecificStringSetting('html', projectContents)
  return setValue ?? 'public/index.html'
}

export function getMainJSFilename(projectContents: ProjectContents): string {
  const setValue = getUtopiaSpecificStringSetting('js', projectContents)
  return setValue ?? 'src/index.js'
}
