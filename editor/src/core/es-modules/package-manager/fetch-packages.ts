import {
  NodeModules,
  esCodeFile,
  esRemoteDependencyPlaceholder,
  ESRemoteDependencyPlaceholder,
  ESCodeFile,
} from '../../shared/project-file-types'
import {
  NpmDependency,
  PackagerServerResponse,
  TypingsServerResponse,
  JsdelivrResponse,
} from '../../shared/npm-dependency-types'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { objectMap } from '../../shared/object-utils'
import { mangleNodeModulePaths, mergeNodeModules } from './merge-modules'
import { getPackagerUrl, getTypingsUrl } from './packager-url'

let depPackagerCache: { [key: string]: NodeModules } = {}

const PACKAGES_TO_SKIP = ['utopia-api', 'react', 'react-dom', 'uuiui', 'uuiui-deps']

function extractNodeModulesFromPackageResponse(response: PackagerServerResponse): NodeModules {
  return objectMap((file) => esCodeFile(file.content, null), response.contents)
}

function extractNodeModulesFromTypingsResponse(response: TypingsServerResponse): NodeModules {
  return mapArrayToDictionary(
    Object.keys(response.files),
    (filepath) => `/node_modules${filepath}`,
    (filepath) => esCodeFile(response.files[filepath].module.code, null),
  )
}

function extractNodeModulesFromJsdelivrResponse(
  packageName: string,
  version: string,
  response: JsdelivrResponse,
): NodeModules {
  const filteredFiles = response.files.filter((file) => file.name.endsWith('.css'))
  return mapArrayToDictionary(
    filteredFiles,
    (file) => `/node_modules/${packageName}${file.name}`,
    (file) => esRemoteDependencyPlaceholder(packageName, version, false),
  )
}

export function resetDepPackagerCache() {
  depPackagerCache = {}
}

// TODO this function is ugly and probably should be split to two functions
// Update: there will be a single request for js and d.ts files, so this will be simplified anyway
async function fetchPackagerResponse(dep: NpmDependency): Promise<NodeModules | null> {
  if (PACKAGES_TO_SKIP.indexOf(dep.name) > -1) {
    return null
  }
  const packagesUrl = getPackagerUrl(dep)
  const jsdelivrUrl = `https://data.jsdelivr.com/v1/package/npm/${dep.name}@${dep.version}/flat`
  let result: NodeModules = {}
  if (depPackagerCache[packagesUrl] != null) {
    result = depPackagerCache[packagesUrl]
  } else {
    const packagerResponse = await fetch(packagesUrl)
    if (packagerResponse.ok) {
      const resp = (await packagerResponse.json()) as PackagerServerResponse
      const convertedResult = extractNodeModulesFromPackageResponse(resp)
      depPackagerCache[packagesUrl] = convertedResult
      result = convertedResult
    }
  }
  if (depPackagerCache[jsdelivrUrl] != null) {
    result = {
      ...result,
      ...depPackagerCache[jsdelivrUrl],
    }
  } else {
    const jsdelivrResponse = await fetch(jsdelivrUrl)
    if (jsdelivrResponse.ok) {
      const resp = (await jsdelivrResponse.json()) as JsdelivrResponse
      const nodeModulesFromResp = extractNodeModulesFromJsdelivrResponse(
        dep.name,
        dep.version,
        resp,
      )
      depPackagerCache[jsdelivrUrl] = nodeModulesFromResp
      result = {
        ...nodeModulesFromResp, // we are deliberately merging this as the lower priority
        ...result, // because if there's a real .js or .d.ts, that should win
      }
    }
  }
  // Note: no error management, imports will show an error
  return result
}

export async function fetchNodeModules(newDeps: Array<NpmDependency>): Promise<NodeModules> {
  const nodeModulesArr = await Promise.all(
    newDeps.map(async (newDep) => {
      try {
        const packagerResponse = await fetchPackagerResponse(newDep)
        if (packagerResponse != null) {
          /**
           * to avoid clashing transitive dependencies,
           * we "move" all transitive dependencies into a subfolder at
           * /node_modules/<main_package>/node_modules/<transitive_dep>/
           *
           * the module resolution won't mind this, the only downside to this approach is
           * that if two main dependencies share the exact same version of a transitive
           * dependency, they will not share that transitive dependency in memory,
           * so this is wasting a bit of memory.
           *
           * but it avoids two of the same transitive dependencies with different versions from
           * overwriting each other.
           *
           * the real nice solution would be to apply npm's module resolution logic that
           * pulls up shared transitive dependencies to the main /node_modules/ folder.
           */
          return mangleNodeModulePaths(newDep.name, packagerResponse)
        } else {
          return {}
        }
      } catch (e) {
        // TODO: proper error handling, now we don't show error for a missing package. The error will be visible when you try to import
        return Promise.resolve({})
      }
    }),
  )
  return mergeNodeModules(nodeModulesArr)
}

function extractFilePath(packagename: string, filepath: string): string {
  const packagenameIndex = filepath.indexOf(packagename)
  const restOfFileUrl = filepath.slice(packagenameIndex + packagename.length)
  return restOfFileUrl
}

export async function fetchMissingFileDependency(
  updateNodeModules: (modulesToAdd: NodeModules) => void,
  dependency: ESRemoteDependencyPlaceholder,
  filepath: string,
): Promise<void> {
  const localFilePath = extractFilePath(dependency.packagename, filepath)
  const jsdelivrUrl = `https://cdn.jsdelivr.net/npm/${dependency.packagename}@${dependency.version}${localFilePath}`
  const jsdelivrResponse = await fetch(jsdelivrUrl)
  const responseAsString = await jsdelivrResponse.text()
  const newFile: ESCodeFile = esCodeFile(responseAsString, null)
  const nodeModulesNewEntry: NodeModules = {
    [filepath]: newFile,
  }
  updateNodeModules(nodeModulesNewEntry)
}
