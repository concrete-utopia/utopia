import * as React from 'react'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { usePropControlledRef_DANGEROUS } from '../../inspector/common/inspector-utils'
import { betterReactMemo, getControlStyles, SelectOption, Utils } from '../../../uuiui-deps'
import * as EP from '../../../core/shared/element-path'
import * as EditorActions from '../../editor/actions/action-creators'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { PopupList } from '../../../uuiui'
import { JSXElementName, jsxElementNameEquals } from '../../../core/shared/element-template'
import { getElementsToTarget } from '../../inspector/common/inspector-utils'
import { Imports } from '../../../core/shared/project-file-types'
import {
  getComponentGroups,
  getComponentGroupsAsSelectOptions,
  InsertableComponent,
} from '../../../components/shared/project-components'
import { usePossiblyResolvedPackageDependencies } from '../../../components/editor/npm-dependency/npm-dependency'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

export const RenderAsRow = betterReactMemo('RenderAsRow', () => {
  const dispatch = useEditorState((store) => {
    return store.dispatch
  }, 'RenderAsRow dispatch')

  const selectedElementName = useEditorState((store) => {
    return MetadataUtils.getJSXElementNameFromMetadata(
      store.editor.selectedViews[0],
      store.editor.jsxMetadata,
    )
  }, 'RenderAsRow selectedElementName')

  const refElementsToTargetForUpdates = useRefEditorState((store) => {
    return getElementsToTarget(store.editor.selectedViews)
  })

  const onElementTypeChange = React.useCallback(
    (newElementName: JSXElementName, importsToAdd: Imports) => {
      const actions = refElementsToTargetForUpdates.current.flatMap((path) => {
        return EditorActions.updateJSXElementName(path, newElementName, importsToAdd)
      })
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates],
  )

  const onSelect = React.useCallback(
    (selectOption: SelectOption) => {
      const value: InsertableComponent = selectOption.value
      onElementTypeChange(value.element.name, value.importsToAdd)
    },
    [onElementTypeChange],
  )

  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo, projectContents, fullPath } = useEditorState(
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
        projectContents: store.editor.projectContents,
        fullPath: store.editor.canvas.openFile?.filename ?? null,
      }
    },
    'RenderAsRow',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return getComponentGroupsAsSelectOptions(
        packageStatus,
        propertyControlsInfo,
        projectContents,
        dependencies,
        fullPath,
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath])

  const currentInsertableComponent: SelectOption | undefined = React.useMemo(() => {
    if (selectedElementName != null) {
      const nameToSearchFor: JSXElementName = selectedElementName
      for (const selectOptionGroup of insertableComponents) {
        for (const selectOption of selectOptionGroup.options ?? []) {
          const insertableComponent: InsertableComponent = selectOption.value
          if (insertableComponent != null) {
            if (jsxElementNameEquals(insertableComponent.element.name, nameToSearchFor)) {
              return selectOption
            }
          }
        }
      }
    }
    return undefined
  }, [insertableComponents, selectedElementName])

  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <span
        style={{
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        Render as
      </span>
      {insertableComponents.length > 0 ? (
        <PopupList
          disabled={false}
          value={currentInsertableComponent}
          onSubmitValue={onSelect}
          options={insertableComponents}
          containerMode='default'
        />
      ) : null}
    </UIGridRow>
  )
})
