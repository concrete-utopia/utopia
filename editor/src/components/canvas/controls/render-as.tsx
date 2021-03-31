import * as React from 'react'
import { getOpenUtopiaJSXComponentsFromState } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { usePropControlledRef_DANGEROUS } from '../../inspector/common/inspector-utils'
import { InstancePath, TemplatePath } from '../../../core/shared/project-file-types'
import { betterReactMemo, getControlStyles, SelectOption, Utils } from '../../../uuiui-deps'
import * as TP from '../../../core/shared/template-path'
import * as EditorActions from '../../editor/actions/action-creators'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { PopupList } from '../../../uuiui'
import { JSXElementName, jsxElementNameEquals } from '../../../core/shared/element-template'
import { useNamesAndIconsAllPaths } from '../../inspector/common/name-and-icon-hook'
import { getElementsToTarget } from '../../inspector/common/inspector-utils'
import { Imports } from '../../../core/shared/project-file-types'
import {
  getComponentGroups,
  getComponentGroupsAsSelectOptions,
  InsertableComponent,
} from '../../../components/shared/project-components'
import { usePossiblyResolvedPackageDependencies } from '../../../components/editor/npm-dependency/npm-dependency'

export const RenderAsRow = betterReactMemo('RenderAsRow', () => {
  const hookResult = useNamesAndIconsAllPaths()
  const constrolStatus = 'simple'
  const controlStyles = getControlStyles(constrolStatus)

  const { dispatch, selectedViews } = useEditorState((store) => {
    return { dispatch: store.dispatch, selectedViews: store.editor.selectedViews }
  }, 'TopMenuContextProvider')

  const refElementsToTargetForUpdates = usePropControlledRef_DANGEROUS(
    getElementsToTarget(selectedViews),
  )

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
    'Name Row Values',
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
    if (hookResult.length > 0 && hookResult[0].name != null) {
      const nameToSearchFor: JSXElementName = hookResult[0].name
      for (const selectOption of insertableComponents) {
        const insertableComponent: InsertableComponent = selectOption.value
        if (jsxElementNameEquals(insertableComponent.element.name, nameToSearchFor)) {
          return selectOption
        }
      }
    }
    return undefined
  }, [insertableComponents, hookResult])

  return (
    <UIGridRow padded={true} type='<---1fr--->|------172px-------|'>
      <span
        style={{
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        Render as
      </span>
      {hookResult.length >= 1 ? (
        <PopupList
          disabled={!controlStyles.interactive}
          value={currentInsertableComponent}
          onSubmitValue={onSelect}
          options={insertableComponents}
          containerMode='default'
        />
      ) : null}
    </UIGridRow>
  )
})
