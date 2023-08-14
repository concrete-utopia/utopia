import React from 'react'

import type { DataRouteObject } from 'react-router'
import { RouterProvider, createMemoryRouter } from 'react-router'

import type { UNSAFE_RouteModules as RouteModules } from '@remix-run/react'
import { UNSAFE_RemixContext as RemixContext } from '@remix-run/react'

import { evaluator } from '../../../core/es-modules/evaluator/evaluator'
import { resolveBuiltInDependency } from '../../../core/es-modules/package-manager/built-in-dependencies'
import type { ProjectContentFile, ProjectContentsTree } from '../../assets'
import { getContentsTreeFileFromString } from '../../assets'
import { useEditorState, Substores } from '../../editor/store/store-hook'
import {
  createAssetsManifest,
  defaultFutureConfig,
  getRoutesFromFiles,
  getTopLevelElement,
  jsxElementUidsPostOrder,
  routeFromEntry,
} from './remix-utils'
import { foldEither } from '../../../core/shared/either'
import { UtopiaRemixRootErrorBoundary } from './utopia-remix-root-error-boundary'
import { patchedCreateReactElement } from '../../../utils/canvas-react-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_PATH_KEY, UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
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

  const builtInDependencies = useEditorState(
    Substores.builtInDependencies,
    (_) => _.builtInDependencies,
    'RemixRootComponent builtInDependencies',
  )

  const routeManifest = React.useMemo(() => {
    const getFlatFilePaths = (root: ProjectContentsTree): ProjectContentFile[] =>
      root.type === 'PROJECT_CONTENT_FILE'
        ? [root]
        : Object.values(root.children).flatMap((c) => getFlatFilePaths(c))

    const flatFiles = Object.values(projectContents).flatMap(getFlatFilePaths)

    return foldEither(
      () => ({}),
      (r) => r,
      getRoutesFromFiles(flatFiles),
    )
  }, [projectContents])

  const assetsManifest = React.useMemo(() => createAssetsManifest(routeManifest), [routeManifest])

  const basePath = props[UTOPIA_PATH_KEY]

  const { routeModules, routes } = React.useMemo(() => {
    const routeManifestResult: RouteModules = {}
    const routesResult: DataRouteObject[] = []
    Object.values(routeManifest).forEach((route) => {
      const contents = getContentsTreeFileFromString(projectContents, route.filePath)
      if (
        contents == null ||
        contents.type !== 'TEXT_FILE' ||
        contents.lastParseSuccess?.type !== 'PARSE_SUCCESS'
      ) {
        return
      }

      const topLevelElement = getTopLevelElement(contents.lastParseSuccess.topLevelElements)
      if (topLevelElement == null) {
        return
      }

      const uidGen = jsxElementUidsPostOrder(topLevelElement.rootElement, [])

      const partialRequire = (toImport: string) => {
        if (toImport === 'react') {
          return {
            ...React,
            createElement: (element: any, propsInner: any, ...children: any) => {
              const uidInfo = uidGen.next()
              if (uidInfo.done) {
                throw new Error('no uid')
              }

              return patchedCreateReactElement(
                element,
                {
                  ...propsInner,
                  [UTOPIA_UID_KEY]: uidInfo.value.uid,
                  [UTOPIA_PATH_KEY]: EP.toString(
                    EP.appendPartToPath(basePath, uidInfo.value.pathPart),
                  ),
                },
                ...children,
              )
            },
          }
        }
        const builtInDependency = resolveBuiltInDependency(builtInDependencies, toImport)
        if (builtInDependency != null) {
          return builtInDependency
        }
        throw new Error(`Cannot resolve dependency: ${toImport}`)
      }

      try {
        const module = evaluator(
          route.filePath,
          contents.fileContents.code,
          { exports: {} },
          partialRequire,
        )

        routeManifestResult[route.id] = {
          default: module.exports.default,
        }

        // HACK LVL: >9000
        // `children` should be filled out properly
        const routeObject: DataRouteObject = {
          ...routeFromEntry(route),
          loader: module.exports.loader != null ? module.exports.loader : undefined,
          action: module.exports.action != null ? module.exports.action : undefined,
        }

        if (routeObject.id === '_index.js') {
          routesResult.find((r) => r.id === 'root')!.children = [routeObject]
        } else {
          routesResult.push(routeObject)
        }
      } catch (e) {
        console.error(e)
      }
    })

    return { routeModules: routeManifestResult, routes: routesResult }
  }, [basePath, builtInDependencies, projectContents, routeManifest])

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
