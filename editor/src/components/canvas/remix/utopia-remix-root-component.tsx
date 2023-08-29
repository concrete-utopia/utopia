import React from 'react'

import { RouterProvider, createMemoryRouter } from 'react-router'

import type { UNSAFE_RouteModules as RouteModules } from '@remix-run/react'

import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { ForceRerunDOMWalkerGLOBAL_SPIKE_KILLME, UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { PathPropHOC, getDefaultExportNameAndUidFromFile, invariant } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import { appendTwoPaths } from '../canvas-utils'

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  // TODO If we make this actually a ref, we don't need the full route modules in here
  const remixDerivedDataRef = useEditorState(
    Substores.derived,
    (store) => store.derived.remixData,
    'UtopiaRemixRootComponent remixData',
  )

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = React.useMemo(() => {
    if (remixDerivedDataRef == null) {
      return null
    }

    const routeModulesResult: RouteModules = {}
    for (const [key, value] of Object.entries(remixDerivedDataRef.routeModuleCreators)) {
      const nameAndUid = getDefaultExportNameAndUidFromFile(
        projectContentsRef.current,
        value.filePath,
      )
      invariant(nameAndUid, 'a default export should be provided')

      const relativePath =
        remixDerivedDataRef.routeModulesToRelativePaths[value.filePath].relativePath

      const defaultComponent = (componentProps: any) =>
        value
          .executionScopeCreator(projectContentsRef.current)
          .scope[nameAndUid.name]?.(componentProps) ?? <React.Fragment />

      routeModulesResult[key] = {
        ...value,
        default: PathPropHOC(defaultComponent, EP.toString(appendTwoPaths(basePath, relativePath))),
      }
    }

    return routeModulesResult
  }, [basePath, remixDerivedDataRef, projectContentsRef])

  const router = React.useMemo(
    () => optionalMap(createMemoryRouter, remixDerivedDataRef?.routes),
    [remixDerivedDataRef],
  )

  let uiJsxCanvasContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  React.useLayoutEffect(() => {
    return router?.subscribe((n) => {
      uiJsxCanvasContext.current.spyValues.metadata = {}
      ForceRerunDOMWalkerGLOBAL_SPIKE_KILLME.current = true
    })
  }, [router, uiJsxCanvasContext])

  if (remixDerivedDataRef == null || router == null || routeModules == null) {
    return null
  }

  const { assetsManifest, futureConfig } = remixDerivedDataRef

  return (
    <RemixContext.Provider
      value={{
        manifest: assetsManifest,
        routeModules: routeModules,
        future: futureConfig,
      }}
    >
      {/* <UtopiaRemixRootErrorBoundary location={location}> */}
      <RouterProvider
        router={router}
        fallbackElement={null}
        future={{ v7_startTransition: true }}
      />
      {/* </UtopiaRemixRootErrorBoundary> */}
    </RemixContext.Provider>
  )
})
