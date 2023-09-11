import type { RouteModules } from '@remix-run/react/dist/routeModules'
import { RemixContext } from '@remix-run/react/dist/components'
import React from 'react'
import { createMemoryRouter, RouterProvider } from 'react-router'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useRefEditorState } from '../../editor/store/store-hook'
import { DefaultExportWithNameAndUid, getDefaultExportNameAndUidFromFile } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import { PathPropHOC } from './path-props-hoc'
import type { RemixDerivedData } from '../../editor/store/remix-derived-data'
import { RemixDerivedDataAtom } from '../../editor/store/remix-derived-data'
import { selectAtom, useAtomCallback } from 'jotai/utils'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { useAtom } from 'jotai'
import { mapDropNulls } from '../../../core/shared/array-utils'

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

const RemixRoutesAtom = selectAtom(
  RemixDerivedDataAtom,
  (remixDerivedData) => remixDerivedData?.routes ?? null,
  shallowEqual,
)

const RemixRouteModuleCreatorsAtom = selectAtom(
  RemixDerivedDataAtom,
  (remixDerivedData) => remixDerivedData?.routeModuleCreators ?? {},
  shallowEqual,
)

function useGetRouteModules(basePath: ElementPath) {
  const remixDerivedDataGetter = useAtomCallback(
    React.useCallback((get) => get(RemixDerivedDataAtom), []),
  )

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const [defaultExports, setDefaultExports] = React.useState<string[]>([])

  useAtomCallback((get) => {
    const routeModuleCreators = get(RemixRouteModuleCreatorsAtom)
    const nameAndUids = mapDropNulls(
      (rmc) =>
        getDefaultExportNameAndUidFromFile(projectContentsRef.current, rmc.filePath)?.name ?? null,
      Object.values(routeModuleCreators),
    )

    setDefaultExports(nameAndUids)
  })

  // React.useEffect(() => {
  //   console.log('defaultExports')
  // }, [defaultExports])

  return React.useMemo(() => {
    const defaultExportsIgnored = defaultExports // Forcibly update the routeModules only when the default exports have changed

    const remixDerivedData = remixDerivedDataGetter()
    if (remixDerivedData == null) {
      return null
    }

    const routeModulesResult: RouteModules = {}
    for (const [routeId, value] of Object.entries(remixDerivedData.routeModuleCreators)) {
      const nameAndUid = getDefaultExportNameAndUidFromFile(
        projectContentsRef.current,
        value.filePath,
      )
      if (nameAndUid == null) {
        continue
      }

      const relativePath = remixDerivedData.routeModulesToRelativePaths[routeId].relativePath

      const defaultComponent = (componentProps: any) =>
        value
          .executionScopeCreator(projectContentsRef.current)
          .scope[nameAndUid.name]?.(componentProps) ?? <React.Fragment />

      routeModulesResult[routeId] = {
        ...value,
        default: PathPropHOC(
          defaultComponent,
          EP.toString(EP.appendTwoPaths(basePath, relativePath)),
        ),
      }
    }

    return routeModulesResult
  }, [basePath, defaultExports, projectContentsRef, remixDerivedDataGetter])
}

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const remixDerivedDataGetter = useAtomCallback(
    React.useCallback((get) => get(RemixDerivedDataAtom), []),
  )

  const [routes] = useAtom(RemixRoutesAtom)

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = useGetRouteModules(basePath)

  // React.useEffect(() => {
  //   console.log('routes')
  // }, [routes])

  // React.useEffect(() => {
  //   console.log('routeModules')
  // }, [routeModules])

  // The router always needs to be updated otherwise new routes won't work without a refresh
  // We need to create the new router with the current location in the initial entries to
  // prevent it thinking that it is rendering '/'
  const router = React.useMemo(() => {
    if (routes == null) {
      return null
    }

    return createMemoryRouter(routes)
  }, [routes])

  const currentRemixDerivedData = remixDerivedDataGetter()

  if (currentRemixDerivedData == null || router == null || routeModules == null) {
    return null
  }

  const { assetsManifest, futureConfig } = currentRemixDerivedData

  return (
    <RemixContext.Provider
      value={{
        manifest: assetsManifest,
        routeModules: routeModules,
        future: futureConfig,
      }}
    >
      <RouterProvider
        router={router}
        fallbackElement={null}
        future={{ v7_startTransition: true }}
      />
    </RemixContext.Provider>
  )
})
