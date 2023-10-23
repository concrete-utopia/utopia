import React from 'react'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import type { SelectOption } from '../../../uuiui-deps'
import * as EditorActions from '../../editor/actions/action-creators'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { PopupList } from '../../../uuiui'
import type { JSXElementName } from '../../../core/shared/element-template'
import { jsxElementNameEquals } from '../../../core/shared/element-template'
import { getElementsToTarget } from '../../inspector/common/inspector-utils'
import type { Imports } from '../../../core/shared/project-file-types'
import type { InsertableComponent } from '../../../components/shared/project-components'
import { getComponentGroupsAsSelectOptions } from '../../../components/shared/project-components'
import { usePossiblyResolvedPackageDependencies } from '../../../components/editor/npm-dependency/npm-dependency'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { useDispatch } from '../../editor/store/dispatch-context'

export const RenderAsRow = React.memo(() => {
  const dispatch = useDispatch()

  const selectedElementName = useEditorState(
    Substores.metadata,
    (store) => {
      return MetadataUtils.getJSXElementNameFromMetadata(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
    },
    'RenderAsRow selectedElementName',
  )

  const refElementsToTargetForUpdates = useRefEditorState((store) => {
    return getElementsToTarget(store.editor.selectedViews)
  })

  const onElementTypeChange = React.useCallback(
    (newElementName: JSXElementName, importsToAdd: Imports) => {
      const actions = refElementsToTargetForUpdates.current.flatMap((path) => {
        return EditorActions.updateJSXElementName(
          path,
          { type: 'JSX_ELEMENT', name: newElementName },
          importsToAdd,
        )
      })
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates],
  )

  const onSelect = React.useCallback(
    (selectOption: SelectOption) => {
      const value: InsertableComponent = selectOption.value
      const element = value.element()
      if (element.type !== 'JSX_ELEMENT') {
        return
      }
      onElementTypeChange(element.name, value.importsToAdd)
    },
    [onElementTypeChange],
  )

  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
      }
    },
    'RenderAsRow',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'RenderAsRow projectContents',
  )

  const fullPath = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.openFile?.filename ?? null,
    'RenderAsRow fullPath',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return getComponentGroupsAsSelectOptions(
        'insert',
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
          const element = insertableComponent.element()
          if (insertableComponent != null && element.type === 'JSX_ELEMENT') {
            if (jsxElementNameEquals(element.name, nameToSearchFor)) {
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
