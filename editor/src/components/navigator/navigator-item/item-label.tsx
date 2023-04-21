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
  const elementRef = React.useRef<HTMLInputElement | null>(null)

  const [name, setName] = React.useState(props.name)

  const isConditionalClause = React.useMemo(() => {
    return props.target.type === 'CONDITIONAL_CLAUSE'
  }, [props.target])

  const isActiveConditionalClause = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isConditionalClauseNavigatorEntry(props.target)) {
        return false
      }
      const parent = findMaybeConditionalExpression(
        props.target.elementPath,
        store.editor.jsxMetadata,
      )
      if (parent == null) {
        return false
      }
      const activeCase = getConditionalActiveCase(
        props.target.elementPath,
        parent,
        store.editor.spyMetadata,
      )
      if (activeCase == null) {
        return false
      }
      return activeCase === props.target.clause
    },
    'NavigatorRowLabel isActiveBranchOfOverriddenConditional',
  )

  React.useEffect(() => {
    if (props.inputVisible && elementRef.current != null) {
      elementRef.current.focus()
      elementRef.current.select()
    }
  }, [props.inputVisible, props.testId])

  const value = React.useMemo(() => {
    return props.suffix == null ? props.name : `${props.name} ${props.suffix}`
  }, [props.suffix, props.name])

  function cancelRename() {
    setName(props.name)
    props.dispatch([EditorActions.setNavigatorRenamingTarget(null)], 'leftpane')
  }

  function triggerRenameComponent() {
    if (isRegularNavigatorEntry(props.target)) {
      // if the name would be the same, or if the new name would be empty, just cancel
      if (props.name === name) {
        cancelRename()
      } else {
        const nameIsBlank = name.trim().length === 0
        const action = renameComponent(props.target.elementPath, nameIsBlank ? null : name)
        props.dispatch([action, EditorActions.setNavigatorRenamingTarget(null)], 'leftpane')
      }
    } else {
      cancelRename()
    }
  }

  function onInputLabelKeyDown(event: React.KeyboardEvent<HTMLInputElement>) {
    if (event.key == 'Enter') {
      triggerRenameComponent()
    }
    if (event.key == 'Escape') {
      cancelRename()
    }
  }

  function onInputLabelBlur() {
    triggerRenameComponent()
  }

  function onInputLabelChange(event: React.ChangeEvent<HTMLInputElement>) {
    setName(event.target.value)
  }

  return (
    <div
      ref={elementRef}
      key='item-label-container'
      className='item-label-container'
      style={{
        ...props.style,
        ...flexRowStyle,
        fontSize: 11,
        fontStyle: props.isDynamic ? 'italic' : 'normal',
      }}
    >
      {isConditionalClause && (
        <Icons.Checkmark
          style={{ opacity: isActiveConditionalClause ? 1 : 0 }}
          color={'secondary'}
        />
      )}
      {props.inputVisible ? (
        <div key='item-rename-label'>
          <StringInput
            key='item-rename-input'
            testId={props.testId}
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
            fontWeight: props.target.type === 'CONDITIONAL_CLAUSE' ? 600 : undefined,
            color: props.target.type === 'CONDITIONAL_CLAUSE' ? colorTheme.fg7.value : undefined,
            textTransform: props.target.type === 'CONDITIONAL_CLAUSE' ? 'uppercase' : undefined,
          }}
          onDoubleClick={(event) => {
            if (!props.isDynamic && event.altKey && isRegularNavigatorEntry(props.target)) {
              props.dispatch(
                [EditorActions.setNavigatorRenamingTarget(props.target.elementPath)],
                'leftpane',
              )
            }
          }}
        >
          {value}
        </div>
      )}
    </div>
  )
})
