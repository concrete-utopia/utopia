import React from 'react'

import { RouterProvider, createMemoryRouter } from 'react-router'

import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { useEditorState, Substores } from '../../editor/store/store-hook'
import {
  PathPropHOC,
  createAssetsManifest,
  defaultFutureConfig,
  getDefaultExportNameAndUidFromFile,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
  invariant,
} from './remix-utils'
import type { Either } from '../../../core/shared/either'
import { foldEither, forEachRight, left } from '../../../core/shared/either'
import { UtopiaRemixRootErrorBoundary } from './utopia-remix-root-error-boundary'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { createExecutionScope } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import type { MutableUtopiaCtxRefData } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { NO_OP } from '../../../core/shared/utils'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  UiJsxCanvasCtxAtom,
  attemptToResolveParsedComponents,
  pickUiJsxCanvasProps,
} from '../ui-jsx-canvas'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import type { MapLike } from 'typescript'
import type { ComponentRendererComponent } from '../ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import * as EP from '../../../core/shared/element-path'

interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const projectContents = useEditorState(
    Substores.projectContents,
    (_) => _.editor.projectContents,
    'RemixRootComponent projectContents',
  )

  const routeManifest = React.useMemo(
    () => createRouteManifestFromProjectContents(projectContents),
    [projectContents],
  )

  let mutableContextRef = React.useRef<MutableUtopiaCtxRefData>({})

  let topLevelComponentRendererComponents = React.useRef<
    MapLike<MapLike<ComponentRendererComponent>>
  >({})

  let resolvedFiles = React.useRef<MapLike<Array<string>>>({}) // Mapping from importOrigin to an array of toImport
  resolvedFiles.current = {}

  let resolvedFileNames = React.useRef<Array<string>>([]) // resolved (i.e. imported) files this render
  resolvedFileNames.current = ['/src/root.js']

  const canvasStuff = useEditorState(
    Substores.fullStore,
    (store) => {
      return pickUiJsxCanvasProps(store.editor, store.derived, NO_OP, NO_OP, NO_OP)
    },
    'CanvasComponentEntry canvasProps',
  )

  let metadataContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  invariant(canvasStuff, "canvasStuff shouldn't be null")

  const requireFn = React.useMemo(
    () => canvasStuff.curriedRequireFn(projectContents),
    [canvasStuff, projectContents],
  )

  const resolve = React.useMemo(
    () => canvasStuff.curriedResolveFn(projectContents),
    [canvasStuff, projectContents],
  )

  const customRequire = React.useCallback(
    (importOrigin: string, toImport: string) => {
      if (resolvedFiles.current[importOrigin] == null) {
        resolvedFiles.current[importOrigin] = []
      }
      let resolvedFromThisOrigin = resolvedFiles.current[importOrigin]

      const alreadyResolved = resolvedFromThisOrigin.includes(toImport) // We're inside a cyclic dependency, so trigger the below fallback
      const filePathResolveResult = alreadyResolved
        ? left<string, string>('Already resolved')
        : resolve(importOrigin, toImport)

      forEachRight(filePathResolveResult, (filepath) => resolvedFileNames.current.push(filepath))

      const resolvedParseSuccess: Either<string, MapLike<any>> = attemptToResolveParsedComponents(
        resolvedFromThisOrigin,
        toImport,
        projectContents,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        '/src/root.js',
        {},
        [],
        [],
        metadataContext,
        NO_OP,
        false,
        filePathResolveResult,
        null,
      )
      return foldEither(
        () => {
          // We did not find a ParseSuccess, fallback to standard require Fn
          return requireFn(importOrigin, toImport, false)
        },
        (scope) => {
          // Return an artificial exports object that contains our ComponentRendererComponents
          return scope
        },
        resolvedParseSuccess,
      )
    },
    [metadataContext, projectContents, requireFn, resolve],
  )

  const rootDefaultExport = React.useMemo(() => {
    const executionScope = createExecutionScope(
      '/src/root.js',
      customRequire,
      mutableContextRef,
      topLevelComponentRendererComponents,
      projectContents,
      '/src/root.js',
      {},
      [],
      [],
      metadataContext,
      NO_OP,
      false,
      null,
    )

    const nameAndUid = getDefaultExportNameAndUidFromFile(projectContents, '/src/root.js')
    invariant(nameAndUid, 'a default export should be provided')

    return PathPropHOC(executionScope.scope[nameAndUid.name], EP.toString(props[UTOPIA_PATH_KEY]))
  }, [customRequire, metadataContext, projectContents, props])

  const indexDefaultExport = React.useMemo(() => {
    const executionScope = createExecutionScope(
      '/src/routes/_index.js',
      customRequire,
      mutableContextRef,
      topLevelComponentRendererComponents,
      projectContents,
      '/src/routes/_index.js',
      {},
      [],
      [],
      metadataContext,
      NO_OP,
      false,
      null,
    )

    const nameAndUid = getDefaultExportNameAndUidFromFile(projectContents, '/src/routes/_index.js')
    invariant(nameAndUid, 'a default export should be provided')

    return PathPropHOC(executionScope.scope[nameAndUid.name], EP.toString(props[UTOPIA_PATH_KEY]))
  }, [customRequire, metadataContext, projectContents, props])

  const assetsManifest = React.useMemo(() => createAssetsManifest(routeManifest), [routeManifest])

  const { routeModules, routes } = React.useMemo(
    () => getRoutesAndModulesFromManifest(routeManifest, rootDefaultExport, indexDefaultExport),
    [indexDefaultExport, rootDefaultExport, routeManifest],
  )

  const router = React.useMemo(() => createMemoryRouter(routes), [routes])

  let [location, setLocation] = React.useState(router.state.location)

  React.useLayoutEffect(() => {
    return router.subscribe((newState) => {
      if (newState.location !== location) {
        setLocation(newState.location)
      }
    })
  }, [location, router])

  return (
    <RemixContext.Provider
      value={{
        manifest: assetsManifest,
        routeModules: routeModules,
        future: defaultFutureConfig,
      }}
    >
      <UtopiaRemixRootErrorBoundary location={location}>
        <RouterProvider
          router={router}
          fallbackElement={null}
          future={{ v7_startTransition: true }}
        />
      </UtopiaRemixRootErrorBoundary>
    </RemixContext.Provider>
  )
})
