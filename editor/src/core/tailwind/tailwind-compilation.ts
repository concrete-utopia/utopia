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
import { ElementsToRerenderGLOBAL } from '../../components/canvas/ui-jsx-canvas'
import { isFeatureEnabled } from '../../utils/feature-switches'
import type { Config } from 'tailwindcss/types/config'
import type { EditorState } from '../../components/editor/store/editor-state'

const LatestConfig: { current: { code: string; config: Config } | null } = { current: null }
export function getTailwindConfigCached(editorState: EditorState): Config | null {
  const tailwindConfig = getProjectFileByFilePath(editorState.projectContents, TailwindConfigPath)
  if (tailwindConfig == null || tailwindConfig.type !== 'TEXT_FILE') {
    return null
  }
  const cached =
    LatestConfig.current == null || LatestConfig.current.code !== tailwindConfig.fileContents.code
      ? null
      : LatestConfig.current.config

  if (cached != null) {
    return cached
  }
  const requireFn = editorState.codeResultCache.curriedRequireFn(editorState.projectContents)
  const customRequire = (importOrigin: string, toImport: string) =>
    requireFn(importOrigin, toImport, false)
  const config = importDefault(customRequire('/', TailwindConfigPath)) as Config
  LatestConfig.current = { code: tailwindConfig.fileContents.code, config: config }

  return config
}

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

function runTailwindClassGenerationOnDOMMutation(
  mutations: MutationRecord[],
  projectContents: ProjectContentTreeRoot,
  requireFn: RequireFn,
) {
  const updateHasNewTailwindData = mutations.some(
    (m) =>
      m.addedNodes.length > 0 || // new DOM element was added with potentially new classes
      m.attributeName === 'class', // a new class was added to the class attribute of an element
  )
  if (!updateHasNewTailwindData) {
    return
  }
  generateTailwindClasses(projectContents, requireFn)
}

export const useTailwindCompilation = (requireFn: RequireFn) => {
  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'useTailwindCompilation projectContents',
  )

  const isInteractionActiveRef = useRefEditorState((store) =>
    interactionSessionIsActive(store.editor.canvas.interactionSession),
  )

  React.useEffect(() => {
    const tailwindConfigFile = getProjectFileByFilePath(projectContents, TailwindConfigPath)
    if (
      tailwindConfigFile == null ||
      isInteractionActiveRef.current ||
      ElementsToRerenderGLOBAL.current !== 'rerender-all-elements' || // implies that an interaction is in progress
      !isFeatureEnabled('Tailwind')
    ) {
      return
    }
    const observer = new MutationObserver((mutations) => {
      runTailwindClassGenerationOnDOMMutation(mutations, projectContents, requireFn)
    })

    observer.observe(document.getElementById(CanvasContainerID)!, {
      attributes: true,
      childList: true,
      subtree: true,
    })

    generateTailwindClasses(projectContents, requireFn)

    return () => {
      observer.disconnect()
    }
  }, [isInteractionActiveRef, projectContents, requireFn])
}
