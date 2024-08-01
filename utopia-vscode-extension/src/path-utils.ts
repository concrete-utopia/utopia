import { Uri } from 'vscode'
import {
  appendToPath,
  stripLeadingSlash,
  stripTrailingSlash,
  toUtopiaPath,
} from 'utopia-vscode-common'

export function addSchemeToPath(projectID: string, path: string): Uri {
  return Uri.parse(toUtopiaPath(projectID, path))
}

export function allPathsUpToPath(path: string): Array<string> {
  const directories = path.split('/')
  const { paths: allPaths } = directories.reduce(
    ({ paths, workingPath }, next) => {
      const nextPath = appendToPath(workingPath, next)
      return {
        paths: paths.concat(nextPath),
        workingPath: nextPath,
      }
    },
    { paths: ['/'], workingPath: '/' },
  )
  return allPaths
}

export function getParentPath(path: string): string | null {
  const withoutLeadingOrTrailingSlash = stripLeadingSlash(stripTrailingSlash(path))
  const pathElems = withoutLeadingOrTrailingSlash.split('/')
  if (pathElems.length <= 1) {
    return null
  } else {
    return `/${pathElems.slice(0, -1).join('/')}`
  }
}
