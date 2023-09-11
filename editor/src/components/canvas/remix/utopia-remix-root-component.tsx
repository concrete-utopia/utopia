import type { RouteModules } from '@remix-run/react/dist/routeModules'
import { RemixContext } from '@remix-run/react/dist/components'
import React from 'react'
import { createMemoryRouter, RouterProvider } from 'react-router'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useRefEditorState } from '../../editor/store/store-hook'
import { getDefaultExportNameAndUidFromFile } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import { PathPropHOC } from './path-props-hoc'
import type { RemixDerivedData } from '../../editor/store/remix-derived-data'
import { RemixDerivedDataAtom } from '../../editor/store/remix-derived-data'
import { atom, useAtom } from 'jotai'

export interface UtopiaRemixRootComponentProps {
  [UTOPIA_PATH_KEY]: ElementPath
}

const remixDerivedDataRef: { current: RemixDerivedData | null } = { current: null }
const RemixDerivedDataRefAtom = atom((get) => {
  remixDerivedDataRef.current = get(RemixDerivedDataAtom)
})

const RemixRoutesAtom = atom((get) => get(RemixDerivedDataAtom)?.routes ?? null)
const RemixRouteModuleCreators = atom((get) => get(RemixDerivedDataAtom)?.routeModuleCreators ?? {})

let rerenders = 0

export const UtopiaRemixRootComponent = React.memo((props: UtopiaRemixRootComponentProps) => {
  const _ = useAtom(RemixDerivedDataRefAtom)

  // console.log(`rerenders: ${rerenders}`)
  // rerenders += 1

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const [routes] = useAtom(RemixRoutesAtom)
  const [routeModuleCreators] = useAtom(RemixRouteModuleCreators)

  const defaultExports = React.useMemo(() => {
    return Object.values(routeModuleCreators).map(
      (rmc) => getDefaultExportNameAndUidFromFile(projectContentsRef.current, rmc.filePath)?.name,
    )
  }, [projectContentsRef, routeModuleCreators])

  const basePath = props[UTOPIA_PATH_KEY]

  const routeModules = React.useMemo(() => {
    const defaultExportsIgnored = defaultExports // Forcibly update the routeModules only when the default exports have changed

    if (remixDerivedDataRef.current == null) {
      return null
    }

    const routeModulesResult: RouteModules = {}
    for (const [routeId, value] of Object.entries(
      remixDerivedDataRef.current.routeModuleCreators,
    )) {
      const nameAndUid = getDefaultExportNameAndUidFromFile(
        projectContentsRef.current,
        value.filePath,
      )
      if (nameAndUid == null) {
        continue
      }

      const relativePath =
        remixDerivedDataRef.current.routeModulesToRelativePaths[routeId].relativePath

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
  }, [basePath, defaultExports, projectContentsRef])

  // The router always needs to be updated otherwise new routes won't work without a refresh
  // We need to create the new router with the current location in the initial entries to
  // prevent it thinking that it is rendering '/'
  const router = React.useMemo(() => {
    if (routes == null) {
      return null
    }

    return createMemoryRouter(routes)
  }, [routes])

  if (remixDerivedDataRef.current == null || router == null || routeModules == null) {
    return null
  }

  const { assetsManifest, futureConfig } = remixDerivedDataRef.current

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
