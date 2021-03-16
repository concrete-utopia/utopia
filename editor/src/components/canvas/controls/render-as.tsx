import * as React from 'react'
import { getOpenUtopiaJSXComponentsFromState } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { usePropControlledRef_DANGEROUS } from '../../inspector/common/inspector-utils'
import { InstancePath, TemplatePath } from '../../../core/shared/project-file-types'
import { betterReactMemo, getControlStyles, SelectOption, Utils } from '../../../uuiui-deps'
import * as TP from '../../../core/shared/template-path'
import * as EditorActions from '../../editor/actions/action-creators'
import { GridRow } from '../../inspector/widgets/grid-row'
import { PopupList } from '../../../uuiui'
import { JSXElementName, jsxElementName } from '../../../core/shared/element-template'
import { useNamesAndIconsSelectedViews } from '../../inspector/common/name-and-icon-hook'
import { getElementsToTarget } from '../../inspector/common/inspector-utils'
import { Imports } from '../../../core/shared/project-file-types'
import {
  getComponentGroups,
  getComponentGroupsAsSelectOptions,
  InsertableComponent,
} from '../../../shared/project-components'
import { usePossiblyResolvedPackageDependencies } from '../../../editor/npm-dependency/npm-dependency'

export const RenderAsRow = betterReactMemo('RenderAsRow', () => {
  const hookResult = useNamesAndIconsSelectedViews()
  const constrolStatus = 'simple'
  const controlStyles = getControlStyles(constrolStatus)

  const { dispatch, selectedViews } = useEditorState((store) => {
    return { dispatch: store.dispatch, selectedViews: store.editor.selectedViews }
  }, 'InspectorContextProvider')

  const refElementsToTargetForUpdates = usePropControlledRef_DANGEROUS(
    getElementsToTarget(selectedViews),
  )

  const onElementTypeChange = React.useCallback(
    (value: JSXElementName) => {
      const actions = refElementsToTargetForUpdates.current.map((path) => {
        return EditorActions.updateJSXElementName(path, value)
      })
      dispatch(actions, 'everyone')
    },
    [dispatch],
  )

  const onSelect = React.useCallback((selectOption: SelectOption) => {
    const value = selectOption.value
    if (typeof value === 'string') {
      const elementName = jsxElementName(value, [])
      onElementTypeChange(elementName)
    } else {
      onElementTypeChange(value)
    }
  }, hookResult)

  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
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
          value={{ value: hookResult[0].name, label: hookResult[0].label }}
          onSubmitValue={onSelect}
          options={typeOptions}
          containerMode='default'
        />
      ) : null}
    </GridRow>
  )
})

const typeOptions: ReadonlyArray<SelectOption> = [
  {
    value: 'View',
    label: 'View',
    icon: {
      category: 'element',
      type: 'view',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'Rectangle',
    label: 'Rectangle',
    icon: {
      category: 'element',
      type: 'rectangle',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'Ellipse',
    label: 'Ellipse',
    icon: {
      category: 'element',
      type: 'ellipse',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'div',
    label: 'div',
    icon: {
      category: 'element',
      type: 'div',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'span',
    label: 'span',
    icon: {
      category: 'element',
      type: 'div',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: jsxElementName('animated', ['div']),
    label: 'animated.div',
    icon: {
      category: 'element',
      type: 'animated',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'img',
    label: 'Image',
    icon: {
      category: 'element',
      type: 'image',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'Text',
    label: 'Text',
    icon: {
      category: 'element',
      type: 'text',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
]
