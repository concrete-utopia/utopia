import React, { useEffect } from 'react'
import type {
  UNSAFE_FutureConfig as FutureConfig,
  UNSAFE_AssetsManifest as AssetsManifest,
} from '@remix-run/react'
import type { MutableUtopiaCtxRefData } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import type { MapLike } from 'typescript'
import type { ComponentRendererComponent } from '../../canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { DataRouteObject } from 'react-router'
import type { ProjectContentTreeRoot } from '../../assets'
import type {
  RouteIdsToModuleCreators,
  RouteModulesWithRelativePaths,
} from '../../canvas/remix/remix-utils'
import {
  createAssetsManifest,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
} from '../../canvas/remix/remix-utils'
import type { CurriedUtopiaRequireFn, CurriedResolveFn } from '../../custom-code/code-file'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { AllElementProps, CanvasBase64Blobs } from './editor-state'
import { atom, useSetAtom } from 'jotai'
import { Substores, useEditorState } from './store-hook'
import { memoize } from '../../../core/shared/memoize'
import { shallowEqual } from '../../../core/shared/equality-utils'

export interface RemixStaticRoutingTable {
  [rootElementUid: string]: string /* file path */
}

export interface RemixDerivedData {
  futureConfig: FutureConfig
  assetsManifest: AssetsManifest
  routeModuleCreators: RouteIdsToModuleCreators
  routeModulesToRelativePaths: RouteModulesWithRelativePaths
  routes: Array<DataRouteObject>
  routingTable: RemixStaticRoutingTable
}

const CreateRemixDerivedDataRefs: {
  mutableContext: { current: MutableUtopiaCtxRefData }
  topLevelComponentRendererComponents: { current: MapLike<MapLike<ComponentRendererComponent>> }
  resolvedFiles: { current: MapLike<Array<string>> }
  resolvedFileNames: { current: Array<string> }
} = {
  mutableContext: { current: {} },
  topLevelComponentRendererComponents: { current: {} },
  resolvedFiles: { current: {} },
  resolvedFileNames: { current: [] },
}

const defaultFutureConfig: FutureConfig = {
  v2_dev: true,
  unstable_postcss: false,
  unstable_tailwind: false,
  v2_errorBoundary: false,
  v2_headers: false,
  v2_meta: false,
  v2_normalizeFormMethod: false,
  v2_routeConvention: true,
}

export function createRemixDerivedData(
  projectContents: ProjectContentTreeRoot,
  spyMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  fileBlobs: CanvasBase64Blobs,
  displayNoneInstances: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  curriedRequireFn: CurriedUtopiaRequireFn,
  curriedResolveFn: CurriedResolveFn,
): RemixDerivedData | null {
  // console.log('createRemixDerivedData')
  const routeManifest = createRouteManifestFromProjectContents(projectContents)
  if (routeManifest == null) {
    return null
  }

  const assetsManifest = createAssetsManifest(routeManifest)

  const metadataCtx = {
    current: {
      spyValues: { metadata: spyMetadata, allElementProps: allElementProps },
    },
  }

  const routesAndModulesFromManifestResult = getRoutesAndModulesFromManifest(
    routeManifest,
    defaultFutureConfig,
    curriedRequireFn,
    curriedResolveFn,
    metadataCtx,
    projectContents,
    CreateRemixDerivedDataRefs.mutableContext,
    CreateRemixDerivedDataRefs.topLevelComponentRendererComponents,
    fileBlobs,
    displayNoneInstances,
    hiddenInstances,
  )

  if (routesAndModulesFromManifestResult == null) {
    return null
  }

  const { routeModuleCreators, routes, routeModulesToRelativePaths, routingTable } =
    routesAndModulesFromManifestResult

  if (routes.length === 0) {
    return null
  }

  return {
    futureConfig: defaultFutureConfig,
    routes: routes,
    assetsManifest: assetsManifest,
    routeModuleCreators: routeModuleCreators,
    routeModulesToRelativePaths: routeModulesToRelativePaths,
    routingTable: routingTable,
  }
}

export const RemixDerivedDataAtom = atom<RemixDerivedData | null>(null)

const createRemixDerivedDataMemo = memoize(createRemixDerivedData, { maxSize: 1 })

// const usePrevious = (value, initialValue) => {
//   const ref = React.useRef(initialValue)
//   React.useEffect(() => {
//     ref.current = value
//   })
//   return ref.current
// }

// const useEffectDebugger = (effectHook, dependencies, dependencyNames = []) => {
//   const previousDeps = usePrevious(dependencies, [])

//   const changedDeps = dependencies.reduce((accum, dependency, index) => {
//     if (dependency !== previousDeps[index]) {
//       const keyName = dependencyNames[index] || index
//       return {
//         ...accum,
//         [keyName]: {
//           before: previousDeps[index],
//           after: dependency,
//         },
//       }
//     }

//     return accum
//   }, {})

//   if (Object.keys(changedDeps).length) {
//     console.log('[use-effect-debugger] ', changedDeps)
//   }

//   React.useEffect(effectHook, dependencies)
// }

export function useGenerateRemixDerivedData(): void {
  const setRemixDerivedData = useSetAtom(RemixDerivedDataAtom)
  const {
    projectContents,
    spyMetadata,
    allElementProps,
    fileBlobs,
    hiddenInstances,
    displayNoneInstances,
    curriedRequireFn,
    curriedResolveFn,
  } = useEditorState(
    Substores.fullStore,
    (store) => ({
      projectContents: store.editor.projectContents,
      spyMetadata: store.editor.spyMetadata,
      allElementProps: store.editor.allElementProps,
      fileBlobs: store.editor.canvas.base64Blobs,
      hiddenInstances: store.editor.hiddenInstances,
      displayNoneInstances: store.editor.displayNoneInstances,
      curriedRequireFn: store.editor.codeResultCache.curriedRequireFn,
      curriedResolveFn: store.editor.codeResultCache.curriedResolveFn,
    }),
    'useRemixDerivedData projectContents',
  )

  // useEffectDebugger(() => {
  //   console.log('useEffectDebugger')
  // }, [
  //   projectContents,
  //   spyMetadata,
  //   allElementProps,
  //   fileBlobs,
  //   hiddenInstances,
  //   displayNoneInstances,
  //   curriedRequireFn,
  //   curriedResolveFn,
  // ])

  const remixDerivedData = createRemixDerivedDataMemo(
    projectContents,
    spyMetadata,
    allElementProps,
    fileBlobs,
    hiddenInstances,
    displayNoneInstances,
    curriedRequireFn,
    curriedResolveFn,
  )

  // console.log('useGenerateRemixDerivedData')

  React.useEffect(
    () => setRemixDerivedData(remixDerivedData),
    [remixDerivedData, setRemixDerivedData],
  )
}
