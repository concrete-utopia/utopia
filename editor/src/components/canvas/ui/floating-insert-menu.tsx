import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'

import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import type {
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
} from '../../shared/project-components'
import {
  getInsertableGroupLabel,
  getNonEmptyComponentGroups,
  isInsertableVariable,
} from '../../shared/project-components'
import { optionalMap } from '../../../core/shared/optional-utils'
import type { InsertMenuMode } from './floating-insert-menu-helpers'
import {
  convertVariablesToElements,
  getVariablesInScope,
} from '../../../components/shared/scoped-variables'

export type InsertMenuItemValue = InsertableComponent & {
  source: InsertableComponentGroupType | null
  key: string
}

export type InsertMenuItem = {
  label: string
  source: string | null
  value: InsertMenuItemValue
}

export type InsertMenuItemGroup = {
  label: string
  options: Array<InsertMenuItem>
}

export type InsertableComponentFlatList = Array<InsertMenuItemGroup>

function convertInsertableComponentsToFlatList(
  insertableComponents: InsertableComponentGroup[],
): InsertableComponentFlatList {
  return insertableComponents.map((componentGroup) => {
    return {
      label: getInsertableGroupLabel(componentGroup.source),
      options: componentGroup.insertableComponents.map(
        (componentToBeInserted, index): InsertMenuItem => {
          const source = index === 0 ? componentGroup.source : null
          let label = componentToBeInserted.name
          // there is no indentation, so for inner props we want to show here the full path (i.e myObj.myProp)
          if (
            isInsertableVariable(componentToBeInserted) &&
            componentToBeInserted.originalName != null
          ) {
            label = componentToBeInserted.originalName
          }
          return {
            label: label,
            source: optionalMap(getInsertableGroupLabel, source),
            value: {
              ...componentToBeInserted,
              key: `${getInsertableGroupLabel(componentGroup.source)}-${label}`,
              source: source,
            },
          }
        },
      ),
    }
  })
}

export function useGetInsertableComponents(
  insertMenuMode: InsertMenuMode,
): InsertableComponentFlatList {
  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
      }
    },
    'useGetInsertableComponents',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'useGetInsertableComponents projectContents',
  )

  const fullPath = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.openFile?.filename ?? null,
    'useGetInsertableComponents fullPath',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return convertInsertableComponentsToFlatList(
        getNonEmptyComponentGroups(
          insertMenuMode,
          packageStatus,
          propertyControlsInfo,
          projectContents,
          dependencies,
          fullPath,
        ),
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath, insertMenuMode])

  const scopedVariables = useEditorState(
    Substores.variablesInScope,
    (store) =>
      getVariablesInScope(
        store.editor.selectedViews[0],
        projectContents,
        store.editor.variablesInScope,
        store.editor.jsxMetadata,
      ),
    'useGetInsertableComponents scopedVariables',
  )

  const insertableVariables = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return convertInsertableComponentsToFlatList(convertVariablesToElements(scopedVariables))
    }
  }, [fullPath, scopedVariables])

  if (insertMenuMode === 'insert') {
    return insertableComponents.concat(insertableVariables)
  } else {
    return insertableComponents
  }
}
