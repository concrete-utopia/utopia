import type { ProjectContentTreeRoot } from '../../components/assets'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import { getProjectFileByFilePath } from '../../components/assets'
import type { Either } from '../shared/either'
import { isRight, left, right } from '../shared/either'
import type { RequireFn } from '../shared/npm-dependency-types'
import type { ProjectFile } from '../shared/project-file-types'
import { isTextFile } from '../shared/project-file-types'
import type { Sheet } from '@twind/core'
import React from 'react'
import { includesDependency } from '../../components/editor/npm-dependency/npm-dependency'
import { memoize } from '../shared/memoize'
import { importDefault } from '../es-modules/commonjs-interop'
import { PostCSSPath, TailwindConfigPath } from './tailwind-config'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import type { Tailwindcss } from '@mhsdesign/jit-browser-tailwindcss'
import { createTailwindcss, type TailwindConfig } from '@mhsdesign/jit-browser-tailwindcss'
import { optionalMap } from '../shared/optional-utils'

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

export const TAILWIND_RAW_CONFIG: { current: Record<string, unknown> | null } = { current: null }

function getTailwindConfig(
  tailwindFile: ProjectFile | null,
  requireFn: RequireFn,
): Either<any, TailwindConfig> {
  if (tailwindFile != null && isTextFile(tailwindFile)) {
    try {
      const requireResult = requireFn('/', TailwindConfigPath)
      const rawConfig = importDefault(requireResult)
      TAILWIND_RAW_CONFIG.current = rawConfig as any
      if (rawConfig != null) {
        return right(rawConfig as TailwindConfig) // TODO as
      } else {
        return left('Tailwind config contains no default export')
      }
    } catch (error) {
      console.error('Error loading tailwind config', error)
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
): TailwindConfig | null {
  const tailwindConfigFile = useGetTailwindConfigFile(projectContents)
  const tailwindConfig = React.useMemo(() => {
    const maybeConfig = getTailwindConfig(tailwindConfigFile, requireFn)
    if (isRight(maybeConfig)) {
      return maybeConfig.value
    } else {
      return null
    }
  }, [tailwindConfigFile, requireFn])
  return useKeepReferenceEqualityIfPossible(tailwindConfig)
}

interface TailwindInstance {
  element: HTMLStyleElement
  instance: Tailwindcss
  intervalHandle: ReturnType<typeof setInterval>
}

let twindInstance: TailwindInstance | null = null

export function isTwindEnabled(): boolean {
  return twindInstance != null
}

function clearTwind() {
  if (twindInstance != null) {
    twindInstance.element.parentNode?.removeChild(twindInstance.element)
    clearTimeout(twindInstance.intervalHandle)
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

async function tailwindCSSUpdate(
  instance: Tailwindcss,
  element: HTMLElement,
  prefixSelector: string,
) {
  const contentElements = document.querySelectorAll(prefixSelector)

  const content = Array.from(contentElements).reduce((carry, el) => carry + el.outerHTML, '')

  element.textContent = await instance.generateStylesFromContent(
    `
  @tailwind base;
  @tailwind components;
  @tailwind utilities;
  `,
    [content],
  )
}

function updateTwind(config: TailwindConfig | null, prefixSelector: string) {
  const element = document.head.appendChild(document.createElement('style'))
  element.appendChild(document.createTextNode('')) // Avoid Edge bug where empty style elements doesn't create sheets
  element.setAttribute('id', `twind-styles-${Math.random().toString(36).slice(2)}`)

  clearTwind()

  if (twindInstance == null) {
    const prefixes = ['TWIND_', 'TAILWIND_']
    window.addEventListener('warning', (event: any | { detail: { code: string } }) => {
      const isTwindWarning: boolean = prefixes.some((prefix) =>
        event?.detail?.code?.startsWith?.(prefix),
      )
      if (isTwindWarning) {
        event.preventDefault()
      }
    })
  }

  // console.log('>>', 'updateTwind')

  // const instance = createTailwindcss(
  //   optionalMap((tailwindConfig) => ({ tailwindConfig }), config) ?? undefined,
  // )

  // twindInstance = {
  //   element: element,
  //   instance: instance,
  //   intervalHandle: setInterval(() => console.log('hi'), 1000), // tailwindCSSUpdate(instance, element, prefixSelector)
  // }
}

export function useTwind(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
  prefixSelector: string,
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
  prefixSelector: string,
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
  const tailwindConfig = isRight(maybeTailwindConfig) ? maybeTailwindConfig.value : null
  if (shouldUseTwind) {
    updateTwind(tailwindConfig, prefixSelector)
  } else {
    clearTwind()
  }
}
