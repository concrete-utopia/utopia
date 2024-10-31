import React from 'react'
import type { TailwindConfig, Tailwindcss } from '@mhsdesign/jit-browser-tailwindcss'
import { createTailwindcss } from '@mhsdesign/jit-browser-tailwindcss'
import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { getProjectFileByFilePath, walkContentsTree } from '../../components/assets'
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
import { createSelector } from 'reselect'
import type { ProjectContentSubstate } from '../../components/editor/store/store-hook-substore-types'

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

const TAILWIND_INSTANCE: {
  current: Tailwindcss | null
  config: Config | null
} = { current: null, config: null }

export function getTailwindConfigFromSingletonInstance(): Config | null {
  return TAILWIND_INSTANCE.config
}

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
  const rawConfig = importDefault(requireFn('/', TailwindConfigPath)) as Config
  const tailwindCss = createTailwindcss({ tailwindConfig: rawConfig })
  TAILWIND_INSTANCE.current = tailwindCss
  TAILWIND_INSTANCE.config = rawConfig
  void generateTailwindStyles(tailwindCss, allCSSFiles)
}

function runTailwindClassGenerationOnDOMMutation(
  mutations: MutationRecord[],
  projectContents: ProjectContentTreeRoot,
  isInteractionActive: boolean,
  requireFn: RequireFn,
) {
  const updateHasNewTailwindData = mutations.some(
    (m) =>
      m.addedNodes.length > 0 || // new DOM element was added with potentially new classes
      m.attributeName === 'class', // potentially new classes were added to the class attribute of an element
  )
  if (
    !updateHasNewTailwindData ||
    isInteractionActive ||
    ElementsToRerenderGLOBAL.current !== 'rerender-all-elements' // implies that an interaction is in progress)
  ) {
    return
  }
  generateTailwindClasses(projectContents, requireFn)
}

const tailwindConfigSelector = createSelector(
  (store: ProjectContentSubstate) => store.editor.projectContents,
  (projectContents) => getProjectFileByFilePath(projectContents, TailwindConfigPath),
)

export const useTailwindCompilation = () => {
  const requireFnRef = useRefEditorState((store) => {
    const requireFn = store.editor.codeResultCache.curriedRequireFn(store.editor.projectContents)
    return (importOrigin: string, toImport: string) => requireFn(importOrigin, toImport, false)
  })
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const isInteractionActiveRef = useRefEditorState(
    (store) => store.editor.canvas.interactionSession != null,
  )

  // this is not a ref, beacuse we want to re-compile the Tailwind classes when the tailwind config changes
  const tailwindConfig = useEditorState(
    Substores.projectContents,
    tailwindConfigSelector,
    'useTailwindCompilation tailwindConfig',
  )

  React.useEffect(() => {
    const canvasContainer = document.getElementById(CanvasContainerID)
    if (
      tailwindConfig == null || // TODO: read this from the utopia key in package.json
      canvasContainer == null ||
      !isFeatureEnabled('Tailwind')
    ) {
      return
    }

    const observer = new MutationObserver((mutations) => {
      runTailwindClassGenerationOnDOMMutation(
        mutations,
        projectContentsRef.current,
        isInteractionActiveRef.current,
        requireFnRef.current,
      )
    })

    observer.observe(canvasContainer, {
      attributes: true,
      childList: true,
      subtree: true,
    })

    // run the initial tailwind class generation
    generateTailwindClasses(projectContentsRef.current, requireFnRef.current)

    return () => {
      observer.disconnect()
    }
  }, [isInteractionActiveRef, projectContentsRef, requireFnRef, tailwindConfig])
}
