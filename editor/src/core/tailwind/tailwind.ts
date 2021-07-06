import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../components/assets'
import { Either, isRight, left, right } from '../shared/either'
import { RequireFn } from '../shared/npm-dependency-types'
import { isTextFile, ProjectFile } from '../shared/project-file-types'
import { Configuration, Sheet, silent } from 'twind'
import { create, observe, cssomSheet, TwindObserver } from 'twind/observe'
import * as React from 'react'
import { packageJsonFileFromProjectContents } from '../../components/editor/store/editor-state'
import { includesDependency } from '../../components/editor/npm-dependency/npm-dependency'
import { propOrNull } from '../shared/object-utils'
import { memoize } from '../shared/memoize'

const PostCSSPath = '/postcss.config.js'
const TailwindConfigPath = '/tailwind.config.js'

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
      if (requireResult?.default != null) {
        return requireResult?.default?.plugins?.tailwindcss != null
      }
    } catch (e) {
      /* Do nothing */
    }
  }

  return false
}

function useGetPostCSSConfigFile(projectContents: ProjectContentTreeRoot): ProjectFile | null {
  return React.useMemo(() => getContentsTreeFileFromString(projectContents, PostCSSPath), [
    projectContents,
  ])
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
      if (requireResult?.default != null) {
        const twindConfig = convertTailwindToTwindConfig(requireResult.default)
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
  return React.useMemo(() => getContentsTreeFileFromString(projectContents, TailwindConfigPath), [
    projectContents,
  ])
}

function useGetTailwindConfig(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
): Configuration {
  const tailwindConfigFile = useGetTailwindConfigFile(projectContents)
  return React.useMemo(() => {
    const maybeConfig = getTailwindConfig(tailwindConfigFile, requireFn)
    if (isRight(maybeConfig)) {
      return maybeConfig.value
    } else {
      return {}
    }
  }, [tailwindConfigFile, requireFn])
}

interface TwindInstance {
  element: HTMLStyleElement
  observer: TwindObserver
}

let twindInstance: TwindInstance | null = null

function clearTwind() {
  if (twindInstance != null) {
    twindInstance.observer.disconnect()
    twindInstance.element.parentNode?.removeChild(twindInstance.element)
  }
}

const adjustRuleScope = memoize(
  (rule: string, prefixSelector: string | null): string => {
    if (prefixSelector == null) {
      return rule
    } else {
      const splitOnBrace = rule.split('{')
      const splitSelectors = splitOnBrace[0].split(',')
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
      const afterBrace = splitOnBrace.slice(1)
      const finalRule = [joinedSelectors, ...afterBrace].join('{')
      return finalRule
    }
  },
  { maxSize: 100, equals: (a, b) => a === b },
)

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
) {
  const hasDependencies = useHasRequiredDependenciesForTailwind(projectContents)
  const hasPostCSSPlugin = usePostCSSIncludesTailwindPlugin(projectContents, requireFn)
  const shouldUseTwind = hasDependencies && hasPostCSSPlugin
  const tailwindConfig = useGetTailwindConfig(projectContents, requireFn)
  React.useMemo(() => {
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
) {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const hasDependencies =
    packageJsonFile != null && hasRequiredDependenciesForTailwind(packageJsonFile)
  const postCSSFile = getContentsTreeFileFromString(projectContents, PostCSSPath)
  const hasPostCSSPlugin =
    postCSSFile != null && postCSSIncludesTailwindPlugin(postCSSFile, requireFn)
  const shouldUseTwind = hasDependencies && hasPostCSSPlugin
  const tailwindConfigFile = getContentsTreeFileFromString(projectContents, TailwindConfigPath)
  const maybeTailwindConfig = getTailwindConfig(tailwindConfigFile, requireFn)
  const tailwindConfig = isRight(maybeTailwindConfig) ? maybeTailwindConfig.value : {}
  if (shouldUseTwind) {
    updateTwind(tailwindConfig, prefixSelector)
  } else {
    clearTwind()
  }
}
