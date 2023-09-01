import type { ProjectContentTreeRoot } from '../../components/assets'
import { getProjectFileByFilePath } from '../../components/assets'
import type { Either } from '../shared/either'
import { isRight, left, right } from '../shared/either'
import type { RequireFn } from '../shared/npm-dependency-types'
import type { ProjectFile } from '../shared/project-file-types'
import { isTextFile } from '../shared/project-file-types'
import type { Configuration, Sheet } from 'twind'
import { silent } from 'twind'
import type { TwindObserver } from 'twind/observe'
import { create, observe, cssomSheet } from 'twind/observe'
import React from 'react'
import { packageJsonFileFromProjectContents } from '../../components/editor/store/editor-state'
import { includesDependency } from '../../components/editor/npm-dependency/npm-dependency'
import { propOrNull } from '../shared/object-utils'
import { memoize } from '../shared/memoize'
import { importDefault } from '../es-modules/commonjs-interop'
import { PostCSSPath, TailwindConfigPath } from './tailwind-config'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'

function hasRequiredDependenciesForTailwind(packageJsonFile: ProjectFile): boolean {
  const hasTailwindDependency = includesDependency(packageJsonFile, 'tailwindcss')
  const hasPostCSSDependency = includesDependency(packageJsonFile, 'postcss')

  return hasTailwindDependency && hasPostCSSDependency
}

function useGetPackageJson(projectContents: ProjectContentTreeRoot): ProjectFile | null {
  return React.useMemo(() => packageJsonFileFromProjectContents(projectContents), [projectContents])
}

function useHasRequiredDependenciesForTailwind(projectContents: ProjectContentTreeRoot): boolean {
  const packageJsonFile = useGetPackageJson(projectContents)
  return React.useMemo(() => {
    return packageJsonFile != null && hasRequiredDependenciesForTailwind(packageJsonFile)
  }, [packageJsonFile])
}

function postCSSIncludesTailwindPlugin(postCSSFile: ProjectFile, requireFn: RequireFn): boolean {
  if (isTextFile(postCSSFile)) {
    try {
      const requireResult = requireFn('/', PostCSSPath)
      const rawConfig = importDefault(requireResult)
      const plugins = (rawConfig as any)?.plugins
      return plugins?.tailwindcss != null
    } catch (e) {
      /* Do nothing */
    }
  }

  return false
}

function useGetPostCSSConfigFile(projectContents: ProjectContentTreeRoot): ProjectFile | null {
  return React.useMemo(
    () => getProjectFileByFilePath(projectContents, PostCSSPath),
    [projectContents],
  )
}

function usePostCSSIncludesTailwindPlugin(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
): boolean {
  const postCSSFile = useGetPostCSSConfigFile(projectContents)
  return React.useMemo(() => {
    return postCSSFile != null && postCSSIncludesTailwindPlugin(postCSSFile, requireFn)
  }, [postCSSFile, requireFn])
}

const PreflightKey = 'preflight'

function enablesPreflight(tailwindConfig: any): boolean {
  const corePlugins = propOrNull('corePlugins', tailwindConfig)
  if (corePlugins != null && typeof corePlugins === 'object') {
    if (Array.isArray(corePlugins)) {
      // This means we have an explicit list of all allowed core plugins
      return corePlugins.includes(PreflightKey)
    } else {
      // We have an object that enables or disables specific plugins - all unlisted plugins are enabled
      const explicitDisabled = corePlugins[PreflightKey] === false // Tailwind doesn't use truthiness here
      return !explicitDisabled
    }
  }

  return true
}

function convertTailwindToTwindConfig(tailwindConfig: any): Configuration {
  const preflightEnabled = enablesPreflight(tailwindConfig)

  return {
    ...tailwindConfig,
    preflight: preflightEnabled,
  }
}

function getTailwindConfig(
  tailwindFile: ProjectFile | null,
  requireFn: RequireFn,
): Either<any, Configuration> {
  if (tailwindFile != null && isTextFile(tailwindFile)) {
    try {
      const requireResult = requireFn('/', TailwindConfigPath)
      const rawConfig = importDefault(requireResult)
      if (rawConfig != null) {
        const twindConfig = convertTailwindToTwindConfig(rawConfig)
        return right(twindConfig)
      } else {
        return left('Tailwind config contains no default export')
      }
    } catch (error) {
      return left(error)
    }
  }

  return left('Invalid or missing tailwind config file type')
}

function useGetTailwindConfigFile(projectContents: ProjectContentTreeRoot): ProjectFile | null {
  return React.useMemo(
    () => getProjectFileByFilePath(projectContents, TailwindConfigPath),
    [projectContents],
  )
}

function useGetTailwindConfig(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
): Configuration {
  const tailwindConfigFile = useGetTailwindConfigFile(projectContents)
  const tailwindConfig = React.useMemo(() => {
    const maybeConfig = getTailwindConfig(tailwindConfigFile, requireFn)
    if (isRight(maybeConfig)) {
      return maybeConfig.value
    } else {
      return {}
    }
  }, [tailwindConfigFile, requireFn])
  return useKeepReferenceEqualityIfPossible(tailwindConfig)
}

interface TwindInstance {
  element: HTMLStyleElement
  observer: TwindObserver
}

let twindInstance: TwindInstance | null = null

export function isTwindEnabled(): boolean {
  return twindInstance != null
}

function clearTwind() {
  if (twindInstance != null) {
    twindInstance.observer.disconnect()
    twindInstance.element.parentNode?.removeChild(twindInstance.element)
  }
}

export function adjustRuleScopeImpl(rule: string, prefixSelector: string | null): string {
  // TODO Use css-tree to handle more complex cases. That doesn't seem necessary right now since Tailwind
  // as at 2.2.4 only uses @media and @keyframes
  const isMediaQuery = rule.startsWith('@media')
  const isOtherAtRule = rule.startsWith('@') && !isMediaQuery
  if (prefixSelector == null || isOtherAtRule) {
    return rule
  } else {
    const splitOnBrace = rule.split('{')
    const selectorIndex = isMediaQuery ? 1 : 0
    const splitSelectors = splitOnBrace[selectorIndex].split(',')
    const scopedSelectors = splitSelectors.map((s) => {
      const lowerCaseSelector = s.toLowerCase().trim()

      if (
        lowerCaseSelector === ':root' ||
        lowerCaseSelector === 'html' ||
        lowerCaseSelector === 'head'
      ) {
        return prefixSelector
      } else if (lowerCaseSelector === 'body') {
        return `${prefixSelector} > *`
      } else {
        return `${prefixSelector} ${s}`
      }
    })
    const joinedSelectors = scopedSelectors.join(',')
    const theRest = splitOnBrace.slice(selectorIndex + 1)
    const front = splitOnBrace.slice(0, selectorIndex)
    const finalRule = [...front, joinedSelectors, ...theRest].join('{')
    return finalRule
  }
}

const adjustRuleScope = memoize(adjustRuleScopeImpl, { maxSize: 100, equals: (a, b) => a === b })

function updateTwind(config: Configuration, prefixSelector: string | null) {
  const element = document.head.appendChild(document.createElement('style'))
  element.appendChild(document.createTextNode('')) // Avoid Edge bug where empty style elements doesn't create sheets

  const sheet = cssomSheet({ target: element.sheet ?? undefined })
  const customSheet: Sheet = {
    target: sheet.target,
    insert: (rule, index) => {
      const scopedRule = adjustRuleScope(rule, prefixSelector)
      sheet.insert(scopedRule, index)
    },
  }

  clearTwind()

  const observer = observe(
    document.documentElement,
    create({ ...config, sheet: customSheet, mode: silent }),
  )

  twindInstance = {
    element: element,
    observer: observer,
  }
}

export function useTwind(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
  prefixSelector: string | null = null,
): void {
  const hasDependencies = useHasRequiredDependenciesForTailwind(projectContents)
  const hasPostCSSPlugin = usePostCSSIncludesTailwindPlugin(projectContents, requireFn)
  const shouldUseTwind = hasDependencies && hasPostCSSPlugin
  const tailwindConfig = useGetTailwindConfig(projectContents, requireFn)
  React.useEffect(() => {
    if (shouldUseTwind) {
      updateTwind(tailwindConfig, prefixSelector)
    } else {
      clearTwind()
    }
  }, [prefixSelector, shouldUseTwind, tailwindConfig])
}

export function injectTwind(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
  prefixSelector: string | null = null,
): void {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const hasDependencies =
    packageJsonFile != null && hasRequiredDependenciesForTailwind(packageJsonFile)
  const postCSSFile = getProjectFileByFilePath(projectContents, PostCSSPath)
  const hasPostCSSPlugin =
    postCSSFile != null && postCSSIncludesTailwindPlugin(postCSSFile, requireFn)
  const shouldUseTwind = hasDependencies && hasPostCSSPlugin
  const tailwindConfigFile = getProjectFileByFilePath(projectContents, TailwindConfigPath)
  const maybeTailwindConfig = getTailwindConfig(tailwindConfigFile, requireFn)
  const tailwindConfig = isRight(maybeTailwindConfig) ? maybeTailwindConfig.value : {}
  if (shouldUseTwind) {
    updateTwind(tailwindConfig, prefixSelector)
  } else {
    clearTwind()
  }
}
