import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import {
  RequirementResolutionResult,
  type RequirementCheck,
  type RequirementCheckResult,
} from '../utopia-requirements-types'
import { getProjectDependencies, walkContentsTree } from '../../../../../components/assets'
import { isParseSuccess, isTextFile } from '../../../../../core/shared/project-file-types'
import builtinModules from './builtin-modules.json'

const serverPackagesRestrictionList: RegExp[] = [/^next/, /^remix/, /^astro/, /^svelte/]

export default class CheckServerPackages implements RequirementCheck {
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
    const projectDependencies = getProjectDependencies(projectContents) ?? {}

    // check for server packages in dependencies
    const serverPackages = Object.keys(projectDependencies).filter((packageName) =>
      serverPackagesRestrictionList.some((restriction) => restriction.test(packageName)),
    )
    if (serverPackages.length > 0) {
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'Server packages found',
        resultValue: serverPackages.join(', '),
      }
    }

    // check for node builtins in imports
    const nodeBuiltins: string[] = []
    walkContentsTree(projectContents, (fullPath, file) => {
      if (isTextFile(file)) {
        const parseResult = file.fileContents.parsed
        if (isParseSuccess(parseResult)) {
          for (const importSource of Object.keys(parseResult.imports)) {
            // if it's a node builtin and not shimmed as a dependency, add it to the list
            if (isBuiltinModule(importSource) && projectDependencies[importSource] == null) {
              nodeBuiltins.push(importSource)
            }
          }
        }
      }
    })
    if (nodeBuiltins.length > 0) {
      return {
        resolution: RequirementResolutionResult.Partial,
        resultText: 'Node built-ins found',
        resultValue: nodeBuiltins.join(', '),
      }
    }
    return {
      resolution: RequirementResolutionResult.Passed,
      resultText: 'No server packages found',
    }
  }
}

const moduleSet = new Set(builtinModules)
const NODE_PROTOCOL = 'node:'
function isBuiltinModule(moduleName: string) {
  let moduleNameWithoutNodeProtocol = moduleName
  if (moduleName.startsWith(NODE_PROTOCOL)) {
    moduleNameWithoutNodeProtocol = moduleName.slice(NODE_PROTOCOL.length)
  }

  return moduleSet.has(moduleNameWithoutNodeProtocol)
}
