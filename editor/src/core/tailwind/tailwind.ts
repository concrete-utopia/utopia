import type { ProjectContentTreeRoot } from '../../components/assets'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import { getProjectFileByFilePath } from '../../components/assets'
import type { Either } from '../shared/either'
import { isRight, left, right } from '../shared/either'
import type { RequireFn } from '../shared/npm-dependency-types'
import type { ProjectFile } from '../shared/project-file-types'
import { isTextFile } from '../shared/project-file-types'
import type { Sheet, Twind } from '@twind/core'
import { cssom, observe, defineConfig, tw } from '@twind/core'
import presetAutoprefix from '@twind/preset-autoprefix'
import presetTailwind from '@twind/preset-tailwind'
import React from 'react'
import { includesDependency } from '../../components/editor/npm-dependency/npm-dependency'
import { propOrNull } from '../shared/object-utils'
import { memoize } from '../shared/memoize'
import { importDefault } from '../es-modules/commonjs-interop'
import { PostCSSPath, TailwindConfigPath } from './tailwind-config'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import { twind } from '@twind/core'

type TwindConfigType = ReturnType<typeof defineConfig>

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

function convertTailwindToTwindConfig(tailwindConfig: any): TwindConfigType {
  const preflightEnabled = enablesPreflight(tailwindConfig)

  const twindConfig = defineConfig({
    presets: [
      presetTailwind({
        disablePreflight: !preflightEnabled,
      }),
      presetAutoprefix(),
    ],
    // force pushing tailwind's config to twind
    theme: tailwindConfig.theme,
    darkMode: tailwindConfig.darkMode,
    variants: tailwindConfig.variants,
    preflight: tailwindConfig.preflight,
  })

  return twindConfig
}

function getTailwindConfig(
  tailwindFile: ProjectFile | null,
  requireFn: RequireFn,
): Either<any, TwindConfigType> {
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
): TwindConfigType {
  const tailwindConfigFile = useGetTailwindConfigFile(projectContents)
  const tailwindConfig = React.useMemo(() => {
    const maybeConfig = getTailwindConfig(tailwindConfigFile, requireFn)
    if (isRight(maybeConfig)) {
      return maybeConfig.value
    } else {
      return defineConfig({})
    }
  }, [tailwindConfigFile, requireFn])
  return useKeepReferenceEqualityIfPossible(tailwindConfig)
}

interface TwindInstance {
  element: HTMLStyleElement
  instance: Twind
}

export const twindInstance: { current: TwindInstance | null } = { current: null }

export function isTwindEnabled(): boolean {
  return twindInstance != null
}

function clearTwind() {
  if (twindInstance.current != null) {
    twindInstance.current.instance.clear()
    twindInstance.current.instance.destroy()
    twindInstance.current.element.parentNode?.removeChild(twindInstance.current.element)
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

const adjustRuleScope = memoize(adjustRuleScopeImpl, {
  maxSize: 100,
  matchesArg: (a, b) => a === b,
})

function updateTwind(config: TwindConfigType, prefixSelector: string | null) {
  const element = document.head.appendChild(document.createElement('style'))
  element.appendChild(document.createTextNode('')) // Avoid Edge bug where empty style elements doesn't create sheets
  element.setAttribute('id', `twind-styles-${Math.random().toString(36).slice(2)}`)

  const sheet = cssom(element)
  const customSheet: Sheet = {
    ...sheet,
    target: sheet.target,
    insert: (rule, index, sheetRule) => {
      const scopedRule = adjustRuleScope(rule, prefixSelector)
      sheet.insert(scopedRule, index, sheetRule)
    },
  }

  addTwindListener()

  clearTwind()

  const instance = observe(twind(config, customSheet), document.documentElement)

  twindInstance.current = {
    element: element,
    instance: instance,
  }
}

let added = false
function addTwindListener() {
  if (added) {
    return
  }
  added = true
  const prefixes = ['TWIND_', 'TAILWIND_', '[TWIND_']
  window.addEventListener('warning', (event: any | { detail: { code: string } }) => {
    const isTwindWarning: boolean = prefixes.some((prefix) =>
      event?.detail?.code?.startsWith?.(prefix),
    )
    if (isTwindWarning) {
      event.preventDefault()
    }
  })
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
  const tailwindConfig = isRight(maybeTailwindConfig) ? maybeTailwindConfig.value : defineConfig({})
  if (shouldUseTwind) {
    updateTwind(tailwindConfig, prefixSelector)
  } else {
    clearTwind()
  }
}
