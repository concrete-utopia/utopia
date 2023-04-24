import React, { CSSProperties } from 'react'
import {
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  NavigatorEntry,
} from '../../../components/editor/store/editor-state'
import { colorTheme, flexRowStyle, Icons, StringInput } from '../../../uuiui'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import { renameComponent } from '../actions'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
} from '../../../core/model/conditionals'

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
    return target.type === 'CONDITIONAL_CLAUSE'
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
    'NavigatorRowLabel isActiveBranchOfOverriddenConditional',
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
        <Icons.Checkmark
          style={{ opacity: isActiveConditionalClause ? 1 : 0 }}
          color={'secondary'}
        />
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
          style={{
            backgroundColor: 'transparent',
            paddingTop: 3,
            paddingBottom: 3,
            marginLeft: isConditionalClause ? 2 : 6,
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            gap: 6,
            fontWeight: target.type === 'CONDITIONAL_CLAUSE' ? 600 : undefined,
            color: target.type === 'CONDITIONAL_CLAUSE' ? colorTheme.fg7.value : undefined,
            textTransform: target.type === 'CONDITIONAL_CLAUSE' ? 'uppercase' : undefined,
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
