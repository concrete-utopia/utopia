import type { CSSProperties } from 'react'
import React from 'react'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'
import {
  isConditionalClauseNavigatorEntry,
  isInvalidOverrideNavigatorEntry,
  isRegularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../../components/editor/store/editor-state'
import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  getConditionalFlag,
} from '../../../core/model/conditionals'
import { colorTheme, flexRowStyle, StringInput } from '../../../uuiui'
import type { EditorDispatch, ElementPaste } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { renameComponent } from '../actions'
import type { RemixItemType } from './navigator-item'
import { NavigatorItemTestId } from './navigator-item'
import { useAtom } from 'jotai'
import type { RemixNavigationAtomData } from '../../canvas/remix/utopia-remix-root-component'
import { RemixNavigationAtom } from '../../canvas/remix/utopia-remix-root-component'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { Location } from 'react-router'
import { RemixIndexPathLabel, getRemixLocationLabel } from '../../canvas/remix/remix-utils'

export function itemLabelTestIdForEntry(navigatorEntry: NavigatorEntry): string {
  return `${NavigatorItemTestId(varSafeNavigatorEntryToKey(navigatorEntry))}-label`
}

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
  remixItemType: RemixItemType
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

  const isInvalidOverride = isInvalidOverrideNavigatorEntry(target)

  React.useEffect(() => {
    if (inputVisible && elementRef.current != null) {
      elementRef.current.focus()
      elementRef.current.select()
    }
  }, [inputVisible, testId])

  const maybeLinkTarget = useEditorState(
    Substores.metadata,
    (store) => {
      if (props.remixItemType !== 'link') {
        return null
      }
      return store.editor.allElementProps[EP.toString(props.target.elementPath)]?.['to'] ?? null
    },
    'ItemLabel maybeLinkTarget',
  )

  const [remixNavigationData] = useAtom(RemixNavigationAtom)

  const maybePathForOutlet = React.useMemo(() => {
    if (props.remixItemType !== 'outlet') {
      return null
    }
    return remixSceneLocationFromOutletPath(props.target.elementPath, remixNavigationData)?.pathname
  }, [props.remixItemType, props.target.elementPath, remixNavigationData])

  const label = React.useMemo(() => {
    if (maybeLinkTarget != null) {
      return maybeLinkTarget
    }
    if (maybePathForOutlet != null) {
      return `Outlet: ${getRemixLocationLabel(maybePathForOutlet)}`
    }
    return suffix == null ? name : `Outlet: ${name} ${suffix}`
  }, [maybeLinkTarget, maybePathForOutlet, suffix, name])

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

  const onInputLabelChange = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    setName(event.target.value)
  }, [])

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

  const color = (() => {
    if (isActiveBranchOfOverriddenConditional) {
      return colorTheme.brandNeonPink.value
    }
    if (isActiveConditionalClause) {
      return colorTheme.dynamicBlue.value
    }
    if (isConditionalClause) {
      return colorTheme.fg7.value
    }
    if (isInvalidOverride) {
      return colorTheme.brandNeonPink.value
    }
    return style?.color
  })()

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
        gap: isConditionalClause ? 10 : undefined,
        padding: isConditionalClause ? undefined : '0 4px',
      }}
    >
      {isConditionalClause && (
        <div
          style={{
            width: 18,
            height: 18,
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
          data-testid={itemLabelTestIdForEntry(target)}
          style={{
            padding: '3px 0px',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            gap: 6,
            fontWeight: isConditionalClause ? 600 : undefined,
            color: color,
            textTransform: isConditionalClause ? 'uppercase' : undefined,
          }}
          onDoubleClick={(event) => {
            if (!isDynamic && event.altKey && isRegularNavigatorEntry(target)) {
              dispatch([EditorActions.setNavigatorRenamingTarget(target.elementPath)], 'leftpane')
            }
          }}
        >
          {label}
        </div>
      )}
    </div>
  )
})

// here
function remixSceneLocationFromOutletPath(
  outletPath: ElementPath,
  remixNavigationData: RemixNavigationAtomData,
): Location | null {
  return EP.findAmongAncestorsOfPath(
    outletPath,
    (p) => remixNavigationData[EP.toString(p)]?.location ?? null,
  )
}
