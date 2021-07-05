import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../components/assets'
import { Either, isRight, left, right } from '../shared/either'
import { RequireFn } from '../shared/npm-dependency-types'
import { isTextFile } from '../shared/project-file-types'
import { Configuration, Sheet, silent } from 'twind'
import { create, observe, cssomSheet, TwindObserver } from 'twind/observe'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import * as React from 'react'

export const TailwindConfigPath = '/tailwind.config.js'

export function hasTailwindConfig(projectContents: ProjectContentTreeRoot): boolean {
  const possibleFile = getContentsTreeFileFromString(projectContents, TailwindConfigPath)
  return possibleFile != null && isTextFile(possibleFile)
}

export function getTailwindConfig(
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
): Either<any, unknown> {
  if (hasTailwindConfig(projectContents)) {
    try {
      const requireResult = requireFn('/', TailwindConfigPath)
      return right(requireResult)
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

export function clearTwind() {
  if (twindInstance != null) {
    twindInstance.observer.disconnect()
    twindInstance.element.parentNode?.removeChild(twindInstance.element)
  }
}

export function updateTwind(config: Configuration, prefixSelector: string) {
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
  const tailwindConfig = useKeepReferenceEqualityIfPossible(
    getTailwindConfig(projectContents, requireFn),
  )
  React.useMemo(() => {
    if (isRight(tailwindConfig)) {
      updateTwind((tailwindConfig.value as any).default as any, prefixSelector)
    } else {
      clearTwind()
    }
  }, [tailwindConfig, prefixSelector])
}
