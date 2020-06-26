// Code is mostly copied and modified from https://github.com/caridy/es6-micro-loader (MIT license).
// I had to put the code here, because the code originally is designed to be used as a module loader
// in the project itself. However, I did not want this to load the utopia modules, only the
// modules written by the user in the utopia app itself.
// As a simple solution I just put the modified version of the code here, which can be directly
// imported in the utopia code, and it provides a System object to evaluate SystemJS builds coming
// from TypeScript.
// The biggest modifiaction I made is that I allow our own require function to be injected and
// used as an alternative way to load a module. Why is this needed? Because we still need to resolve
// external npm dependencies in user code, and those are not bundled by the TS compiler.
// So there are 2 ways to resolve a module:
// - either the module is user code, and it is bundled by the TS compiler: then it will be a
//   SystemJS module and will be registered when we evaluate the bundled code.
// - or the module is coming from an external dependency, and it will be resolved by the require
//   function stored in the editor state
//
// I also converted it to Typescript code, so we don't have compiler errors.
// TODO: make it more typescript, I left a lot of 'any'-s inside

import { RequireFn } from '../../core/shared/npm-dependency-types'
import Utils from '../../utils/utils'
import { normalizeName } from './custom-code-utils'
import { RawSourceMap } from '../../core/workers/ts/ts-typings/RawSourceMap'

var seen: { [key: string]: boolean } = {}
var internalRegistry: { [key: string]: any } = {}
var externalRegistry: { [key: string]: any } = {}
var sourceMapRegistry: { [key: string]: any } = {}
var requireFn: RequireFn = Utils.NO_OP
var currentModule: string | null = null
var currentSourceMap: RawSourceMap | null = null

export function reset(reqFn: RequireFn, resetRegistry: boolean) {
  seen = {}
  requireFn = reqFn
  currentModule = null
  if (resetRegistry) {
    internalRegistry = {}
    externalRegistry = {}
    sourceMapRegistry = {}
  }
}

function ensuredExecute(name: string, skipRegistering: boolean) {
  // First we should check if the name is available in a registry (it is a file)
  let mod = internalRegistry[name]
  // Then we should check if the name is a directory and there is an index.js/ts/tsx file inside it
  if (mod == null) {
    mod = internalRegistry[`${name}/index`]
  }
  // Third option would be that there is a package.json inside the directory, but we don't support that
  if (mod && !seen[name]) {
    if (!skipRegistering) {
      seen[name] = true
    }
    // one time operation to execute the module body
    mod.execute()
  }
  return mod && mod.proxy
}

function get(importOrigin: string, name: string, skipRegistering: boolean) {
  const normalizedName = normalizeName(importOrigin, name)
  const importResult =
    externalRegistry[normalizedName] ||
    ensuredExecute(normalizedName, skipRegistering) ||
    requireFn(importOrigin, name)
  if (importResult === undefined) {
    const error = new Error(`Could not find dependency: '${name}' relative to '${importOrigin}'`)
    error.name = 'DependencyNotFoundError'
    throw error
  }
  if (importResult != null && typeof importResult === 'object' && importResult['default'] == null) {
    // allowSyntheticDefaultImports: true
    importResult['default'] = importResult
  }
  return importResult
}

function has(importOrigin: string, name: string) {
  const normalizedName = normalizeName(importOrigin, name)
  try {
    return (
      !!externalRegistry[normalizedName] ||
      !!internalRegistry[normalizedName] ||
      !!requireFn(importOrigin, name)
    )
  } catch {
    // Capturing the case where `requireFn` throws an exception as it should do
    // when something does not exist.
    return false
  }
}

export var System = {
  get: get,
  has: has,
  setModule: function (name: string, sourceMap: RawSourceMap | null) {
    currentModule = normalizeName('/', name)
    if (sourceMap != null) {
      sourceMapRegistry[currentModule] = sourceMap
    }
  },
  getModule: function (importOrigin: string, name: string, skipRegistering: boolean) {
    return get(importOrigin, name, skipRegistering)
  },
  register: function (deps: Array<string>, wrapper: any) {
    if (currentModule == null) {
      throw Error('No module name set')
    }
    const name = currentModule
    var proxy = Object.create(null)
    var values = Object.create(null)
    var mod: any
    var meta: any
    // creating a new entry in the internal registry
    internalRegistry[name] = mod = {
      // live bindings
      proxy: proxy,
      // exported values
      values: values,
      // normalized deps
      deps: deps.map(function (dep) {
        return normalizeName(name, dep)
      }),
      // other modules that depends on this so we can push updates into those modules
      dependants: [],
      // method used to push updates of deps into the module body
      update: function (moduleName: string, moduleObj: any) {
        meta.setters[mod.deps.indexOf(moduleName)](moduleObj)
      },
      execute: function () {
        mod.deps.forEach(function (dep: string) {
          let imports: any
          try {
            imports = externalRegistry[dep] || requireFn('/', dep)
          } catch {
            // tslint:disable-next-line:no-empty
            // Capturing the case where `requireFn` throws an exception as it should do
            // when something does not exist.
          }
          if (imports == null) {
            imports = get('./', dep, false) && internalRegistry[dep].values // optimization to pass plain values instead of bindings
            if (imports != null) {
              internalRegistry[dep].dependants.push(name)
              mod.update(dep, imports)
            }
          } else {
            mod.update(dep, imports)
          }
        })
        try {
          meta.execute()
        } catch (e) {
          const sourceMap = sourceMapRegistry[name]
          if (sourceMap != null) {
            Utils.processErrorWithSourceMap(Utils.NO_OP, e, '', sourceMapRegistry[name], false)
          }
          throw e
        }
      },
    }
    // allowSyntheticDefaultImports: true
    if (internalRegistry[name].values['default'] == null) {
      internalRegistry[name].values['default'] = values
    }
    // collecting execute() and setters[]
    meta = wrapper(function (identifierOrModule: any, value: any) {
      if (typeof identifierOrModule === 'string') {
        values[identifierOrModule] = value
        mod.lock = true // locking down the updates on the module to avoid infinite loop
        mod.dependants.forEach(function (moduleName: string) {
          if (internalRegistry[moduleName] && !internalRegistry[moduleName].lock) {
            internalRegistry[moduleName].update(name, values)
          }
        })
        mod.lock = false
        if (!Object.getOwnPropertyDescriptor(proxy, identifierOrModule)) {
          Object.defineProperty(proxy, identifierOrModule, {
            enumerable: true,
            get: function () {
              return values[identifierOrModule]
            },
          })
        }
        return value
      } else if (typeof identifierOrModule === 'object') {
        Object.keys(identifierOrModule).forEach((key) => {
          if (Object.getOwnPropertyDescriptor(proxy, key) == null) {
            const valueFromModule = identifierOrModule[key]
            values[key] = valueFromModule
            Object.defineProperty(proxy, key, {
              enumerable: true,
              get: function () {
                return valueFromModule
              },
            })
          }
        })
      } else {
        console.warn('Unhandled exported value.', identifierOrModule)
      }
    })
  },
}
