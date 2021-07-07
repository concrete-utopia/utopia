/** @jsx jsx */

import React from 'react'
import { jsx } from '@emotion/react'

import TailWindList from '../../../core/third-party/tailwind-all-classnames.json'
import WindowedSelect from 'react-windowed-select'

import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme, UtopiaTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { jsxAttributeValue } from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'

const TailWindOptions = TailWindList.classNames.map((className, index) => ({
  label: className,
  value: className,
}))

export const ClassNameSelect: React.FunctionComponent = betterReactMemo('ClassNameSelect', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'ClassNameSelect dispatch')

  const selectedElement = useEditorState((store) => {
    const metadata = store.editor.jsxMetadata
    if (store.editor.selectedViews.length === 1) {
      return MetadataUtils.findElementByElementPath(metadata, store.editor.selectedViews[0])
    } else {
      return null
    }
  }, 'ClassNameSelect selectedElement')
  const onSubmitValue = React.useCallback(
    (newValue: Array<{ label: string; value: string }>) => {
      if (selectedElement != null) {
        dispatch(
          [
            EditorActions.setProp_UNSAFE(
              selectedElement.elementPath,
              PP.create(['className']),
              jsxAttributeValue(newValue.map((value) => value.value).join(' '), emptyComments),
            ),
          ],
          'everyone',
        )
      }
    },
    [dispatch, selectedElement],
  )

  const classNames = selectedElement?.props?.className
  const splitClassNames =
    typeof classNames === 'string'
      ? classNames
          .split(' ')
          .map((s) => s.trim())
          .filter((s) => s !== '')
      : []
  const selectedValues =
    splitClassNames.length === 0
      ? null
      : splitClassNames.map((name: string) => ({
          label: name,
          value: name,
        }))

  return (
    <WindowedSelect
      isMulti={true}
      options={TailWindOptions}
      onChange={onSubmitValue}
      value={selectedValues}
    />
  )
})
