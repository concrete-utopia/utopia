import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../components/assets'
import { Either, isRight, left, right } from '../shared/either'
import { RequireFn } from '../shared/npm-dependency-types'
import { isTextFile } from '../shared/project-file-types'
import { Configuration, Sheet, silent } from 'twind'
import { create, observe, cssomSheet, TwindObserver } from 'twind/observe'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import * as React from 'react'

const PostCSSPath = '/postcss.config.js'
const TailwindConfigPath = '/tailwind.config.js'

function hasPostCSSConfig(projectContents: ProjectContentTreeRoot): boolean {
  const possibleFile = getContentsTreeFileFromString(projectContents, PostCSSPath)
  return possibleFile != null && isTextFile(possibleFile)
}

function usesTailwind(projectContents: ProjectContentTreeRoot, requireFn: RequireFn): boolean {
  if (hasPostCSSConfig(projectContents)) {
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

function hasTailwindConfig(projectContents: ProjectContentTreeRoot): boolean {
  const possibleFile = getContentsTreeFileFromString(projectContents, TailwindConfigPath)
  return possibleFile != null && isTextFile(possibleFile)
}

function getTailwindConfig(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
): Either<any, Configuration> {
  if (hasTailwindConfig(projectContents)) {
    try {
      const requireResult = requireFn('/', TailwindConfigPath)
      if (requireResult?.default != null) {
        return right(requireResult.default)
      } else {
        return left('Tailwind config contains no default export')
      }
    } catch (error) {
      return left(error)
    }
  } else {
    return left('No valid tailwind config available.')
  }
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

function updateTwind(config: Configuration, prefixSelector: string) {
  const element = document.head.appendChild(document.createElement('style'))
  element.appendChild(document.createTextNode('')) // Avoid Edge bug where empty style elements doesn't create sheets

  const sheet = cssomSheet({ target: element.sheet ?? undefined })
  const customSheet: Sheet = {
    target: sheet.target,
    insert: (rule, index) => {
      sheet.insert(`${prefixSelector} ${rule}`, index)
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
  prefixSelector: string,
) {
  const shouldUseTwind = usesTailwind(projectContents, requireFn)
  const tailwindConfig = useKeepReferenceEqualityIfPossible(
    getTailwindConfig(projectContents, requireFn),
  )
  React.useMemo(() => {
    if (shouldUseTwind) {
      const configToUse = isRight(tailwindConfig) ? tailwindConfig.value : {}
      updateTwind(configToUse, prefixSelector)
    } else {
      clearTwind()
    }
  }, [prefixSelector, shouldUseTwind, tailwindConfig])
}

// [ ] Map tailwindcss preflight config to twind
// [ ] Hook into preview
