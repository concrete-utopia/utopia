import type { CSSProperties } from 'react'
import React from 'react'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'
import {
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../../components/editor/store/editor-state'
import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  getConditionalFlag,
} from '../../../core/model/conditionals'
import { colorTheme, flexRowStyle, StringInput } from '../../../uuiui'
import type { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { renameComponent } from '../actions'
import { NavigatorItemTestId } from './navigator-item'

interface ItemLabelProps {
  testId: string
  dispatch: EditorDispatch
  target: NavigatorEntry
  isDynamic: boolean
  selected: boolean
  name: string
  suffix?: string
  inputVisible: boolean
  style?: CSSProperties
}

export const ItemLabel = React.memo((props: ItemLabelProps) => {
  const {
    name: propsName,
    testId,
    dispatch,
    target,
    isDynamic,
    suffix,
    inputVisible,
    style,
  } = props

  const elementRef = React.useRef<HTMLInputElement | null>(null)

  const [name, setName] = React.useState(propsName)

  const isConditionalClause = React.useMemo(() => {
    return isConditionalClauseNavigatorEntry(target)
  }, [target])

  const isActiveConditionalClause = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isConditionalClauseNavigatorEntry(target)) {
        return false
      }
      const parent = findMaybeConditionalExpression(target.elementPath, store.editor.jsxMetadata)
      if (parent == null) {
        return false
      }
      const activeCase = getConditionalActiveCase(
        target.elementPath,
        parent,
        store.editor.spyMetadata,
      )
      if (activeCase == null) {
        return false
      }
      return activeCase === target.clause
    },
    'NavigatorItemLabel isActiveBranchOfOverriddenConditional',
  )

  React.useEffect(() => {
    if (inputVisible && elementRef.current != null) {
      elementRef.current.focus()
      elementRef.current.select()
    }
  }, [inputVisible, testId])

  const value = React.useMemo(() => {
    return suffix == null ? name : `${name} ${suffix}`
  }, [suffix, name])

  const cancelRename = React.useCallback(() => {
    setName(name)
    dispatch([EditorActions.setNavigatorRenamingTarget(null)], 'leftpane')
  }, [dispatch, name])

  const triggerRenameComponent = React.useCallback(() => {
    if (isRegularNavigatorEntry(target)) {
      // if the name would be the same, or if the new name would be empty, just cancel
      if (propsName === name) {
        cancelRename()
      } else {
        const nameIsBlank = name.trim().length === 0
        const action = renameComponent(target.elementPath, nameIsBlank ? null : name)
        dispatch([action, EditorActions.setNavigatorRenamingTarget(null)], 'leftpane')
      }
    } else {
      cancelRename()
    }
  }, [cancelRename, target, propsName, dispatch, name])

  const onInputLabelKeyDown = React.useCallback(
    (event: React.KeyboardEvent<HTMLInputElement>) => {
      if (event.key == 'Enter') {
        triggerRenameComponent()
      }
      if (event.key == 'Escape') {
        cancelRename()
      }
    },
    [cancelRename, triggerRenameComponent],
  )

  const onInputLabelBlur = React.useCallback(() => {
    triggerRenameComponent()
  }, [triggerRenameComponent])

  function onInputLabelChange(event: React.ChangeEvent<HTMLInputElement>) {
    setName(event.target.value)
  }

  const isActiveBranchOfOverriddenConditional = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isConditionalClauseNavigatorEntry(target)) {
        return false
      }

      const conditional = findMaybeConditionalExpression(
        target.elementPath,
        store.editor.jsxMetadata,
      )
      if (conditional == null) {
        return false
      }

      switch (getConditionalFlag(conditional)) {
        case true:
          return target.clause === 'true-case'
        case false:
          return target.clause === 'false-case'
        default:
          return false
      }
    },
    'NavigatorItemLabel isActiveBranchOfOverriddenConditional',
  )

  return (
    <div
      ref={elementRef}
      key='item-label-container'
      className='item-label-container'
      style={{
        ...style,
        ...flexRowStyle,
        fontSize: 11,
        fontStyle: isDynamic ? 'italic' : 'normal',
      }}
    >
      {isConditionalClause && (
        <div
          style={{
            width: 16,
            height: 16,
            display: 'flex',
            fontWeight: 'bold',
            alignItems: 'center',
            justifyContent: 'center',
            opacity: isActiveConditionalClause ? 1 : 0,
            color: isActiveBranchOfOverriddenConditional
              ? colorTheme.brandNeonPink.value
              : colorTheme.dynamicBlue.value,
          }}
        >
          ✓
        </div>
      )}
      {inputVisible ? (
        <div key='item-rename-label'>
          <StringInput
            key='item-rename-input'
            testId={testId}
            className='rename-input-field'
            ref={elementRef}
            type='text'
            value={name}
            onKeyDown={onInputLabelKeyDown}
            onBlur={onInputLabelBlur}
            onChange={onInputLabelChange}
          />
        </div>
      ) : (
        <div
          key='item-label'
          data-testid={`${NavigatorItemTestId(varSafeNavigatorEntryToKey(target))}-label`}
          style={{
            backgroundColor: 'transparent',
            paddingTop: 3,
            paddingBottom: 3,
            marginLeft: isConditionalClause ? 4 : 0,
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            gap: 6,
            fontWeight: isConditionalClause ? 600 : undefined,
            color: isActiveBranchOfOverriddenConditional
              ? colorTheme.brandNeonPink.value
              : isConditionalClause && isActiveConditionalClause
              ? colorTheme.dynamicBlue.value
              : isConditionalClause
              ? colorTheme.fg7.value
              : undefined,
            textTransform: isConditionalClause ? 'uppercase' : undefined,
          }}
          onDoubleClick={(event) => {
            if (!isDynamic && event.altKey && isRegularNavigatorEntry(target)) {
              dispatch([EditorActions.setNavigatorRenamingTarget(target.elementPath)], 'leftpane')
            }
          }}
        >
          {value}
        </div>
      )}
    </div>
  )
})
