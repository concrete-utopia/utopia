import * as React from 'react'
import { getOpenUtopiaJSXComponentsFromState } from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'
import { usePropControlledRef_DANGEROUS } from '../../../components/inspector/common/inspector-utils'
import { InstancePath, TemplatePath } from '../../../core/shared/project-file-types'
import { betterReactMemo, getControlStyles, SelectOption, Utils } from '../../../uuiui-deps'
import * as TP from '../../../core/shared/template-path'
import * as EditorActions from '../../editor/actions/action-creators'
import { GridRow } from '../../../components/inspector/widgets/grid-row'
import { PopupList } from '../../../uuiui'
import { JSXElementName, jsxElementName } from '../../../core/shared/element-template'
import { useNamesAndIconsSelectedViews } from '../../../components/inspector/common/name-and-icon-hook'

export const NameRowCrumbs = betterReactMemo('NameRowCrumbs', () => {
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'InspectorEntryPoint selectedViews',
  )

  const refElementsToTargetForUpdates = usePropControlledRef_DANGEROUS(
    getElementsToTarget(selectedViews),
  )

  const { dispatch, jsxMetadataKILLME, rootComponents } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      jsxMetadataKILLME: store.editor.jsxMetadataKILLME,
      rootComponents: getOpenUtopiaJSXComponentsFromState(store.editor),
    }
  }, 'InspectorContextProvider')

  function getElementsToTarget(paths: Array<TemplatePath>): Array<InstancePath> {
    let result: Array<InstancePath> = []
    Utils.fastForEach(paths, (path) => {
      // TODO Scene Implementation
      if (!TP.isScenePath(path) && !TP.containsPath(path, result)) {
        result.push(path)
      }
    })
    return result
  }

  const onElementTypeChange = React.useCallback(
    (value: JSXElementName) => {
      const actions = refElementsToTargetForUpdates.current.map((path) => {
        return EditorActions.updateJSXElementName(path, value)
      })
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates],
  )

  const label: string = 'Test'
  const type: string = 'TestType'
  const hookResult = useNamesAndIconsSelectedViews()

  const constrolStatus = 'simple'
  const controlStyles = getControlStyles(constrolStatus)

  const NameRow = betterReactMemo('NameRow', () => {
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
        {type == null ? null : (
          <PopupList
            disabled={!controlStyles.interactive}
            value={{ value: type, label: type }}
            onSubmitValue={onSelect}
            options={typeOptions}
            containerMode='default'
          />
        )}
      </GridRow>
    )
  })

  return (
    <div>
      <NameRow />
    </div>
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
