import React from 'react'
import type { TailwindConfig, Tailwindcss } from '@mhsdesign/jit-browser-tailwindcss'
import { createTailwindcss } from '@mhsdesign/jit-browser-tailwindcss'
import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { getProjectFileByFilePath, walkContentsTree } from '../../components/assets'
import { interactionSessionIsActive } from '../../components/canvas/canvas-strategies/interaction-state'
import { CanvasContainerID } from '../../components/canvas/canvas-types'
import {
  Substores,
  useEditorState,
  useRefEditorState,
} from '../../components/editor/store/store-hook'
import { importDefault } from '../es-modules/commonjs-interop'
import { rescopeCSSToTargetCanvasOnly } from '../shared/css-utils'
import type { RequireFn } from '../shared/npm-dependency-types'
import { TailwindConfigPath } from './tailwind-config'
import { createSelector } from 'reselect'
import type { ProjectContentSubstate } from '../../components/editor/store/store-hook-substore-types'
import { ElementsToRerenderGLOBAL } from '../../components/canvas/ui-jsx-canvas'

const TAILWIND_INSTANCE: { current: Tailwindcss | null } = { current: null }

export function isTailwindEnabled(): boolean {
  return TAILWIND_INSTANCE.current != null
}

function ensureElementExists({ type, id }: { type: string; id: string }) {
  let tag = document.getElementById(id)
  if (tag == null) {
    tag = document.createElement(type)
    tag.id = id
    document.head.appendChild(tag)
  }
  return tag
}

async function generateTailwindStyles(tailwindCss: Tailwindcss, allCSSFiles: string) {
  const contentElement = document.getElementById(CanvasContainerID)
  if (contentElement == null) {
    return
  }
  const content = contentElement.outerHTML
  const styleString = await tailwindCss.generateStylesFromContent(allCSSFiles, [content])
  const style = ensureElementExists({ type: 'style', id: 'utopia-tailwind-jit-styles' })
  style.textContent = rescopeCSSToTargetCanvasOnly(styleString)
}

function getCssFilesFromProjectContents(projectContents: ProjectContentTreeRoot) {
  let files: string[] = []
  walkContentsTree(projectContents, (path, file) => {
    if (file.type === 'TEXT_FILE' && path.endsWith('.css')) {
      files.push(file.fileContents.code)
    }
  })
  return files
}

function generateTailwindClasses(projectContents: ProjectContentTreeRoot, requireFn: RequireFn) {
  const allCSSFiles = getCssFilesFromProjectContents(projectContents).join('\n')
  const rawConfig = importDefault(requireFn('/', TailwindConfigPath))
  const tailwindCss = createTailwindcss({ tailwindConfig: rawConfig as TailwindConfig })
  TAILWIND_INSTANCE.current = tailwindCss
  void generateTailwindStyles(tailwindCss, allCSSFiles)
}

const projectContentsSelector = createSelector(
  (store: ProjectContentSubstate) => store.editor.projectContents,
  (projectContents: ProjectContentTreeRoot) => projectContents,
)

export const useTailwindCompilation = (requireFn: RequireFn) => {
  const projectContents = useEditorState(
    Substores.projectContents,
    projectContentsSelector,
    'useTailwindCompilation projectContents',
  )

  const isInteractionActiveRef = useRefEditorState((store) =>
    interactionSessionIsActive(store.editor.canvas.interactionSession),
  )

  const observerCallback = React.useCallback(() => {
    if (
      isInteractionActiveRef.current ||
      ElementsToRerenderGLOBAL.current !== 'rerender-all-elements' // implies that an
    ) {
      return
    }
    generateTailwindClasses(projectContents, requireFn)
  }, [isInteractionActiveRef, projectContents, requireFn])

  React.useEffect(() => {
    const tailwindConfigFile = getProjectFileByFilePath(projectContents, TailwindConfigPath)
    if (tailwindConfigFile == null || tailwindConfigFile.type !== 'TEXT_FILE') {
      return // we consider tailwind to be enabled if there's a tailwind config file in the project
    }
    const observer = new MutationObserver(observerCallback)

    observer.observe(document.getElementById(CanvasContainerID)!, {
      attributes: true,
      childList: true,
      subtree: true,
    })

    observerCallback()

    return () => {
      observer.disconnect()
    }
  }, [isInteractionActiveRef, observerCallback, projectContents, requireFn])
}
