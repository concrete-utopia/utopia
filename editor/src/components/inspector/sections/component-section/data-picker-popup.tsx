/** @jsxRuntime classic */
/** @jsxFrag */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useCallback } from 'react'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { PropertyPath } from '../../../../core/shared/project-file-types'
import { useColorTheme, Button, FlexColumn, UtopiaStyles } from '../../../../uuiui'
import { setProp_UNSAFE } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { DataPickerPopupTestId, VariableFromScopeOptionTestId } from './component-section'
import * as EP from '../../../../core/shared/element-path'
import type {
  ArrayInfo,
  JSXInfo,
  ObjectInfo,
  PrimitiveInfo,
  VariableInfo,
} from './variables-in-scope-utils'
import { useVariablesInScopeForSelectedElement } from './variables-in-scope-utils'
import { assertNever } from '../../../../core/shared/utils'

export interface PrimitiveOption {
  type: 'primitive'
  variableInfo: PrimitiveInfo
  definedElsewhere: string
  depth: number
}

export interface ArrayOption {
  type: 'array'
  variableInfo: ArrayInfo
  depth: number
  definedElsewhere: string
  children: Array<VariableOption>
}

export interface ObjectOption {
  type: 'object'
  variableInfo: ObjectInfo
  depth: number
  definedElsewhere: string
  children: Array<VariableOption>
}

export interface JSXOption {
  type: 'jsx'
  variableInfo: JSXInfo
  definedElsewhere: string
  depth: number
}

export type VariableOption = PrimitiveOption | ArrayOption | ObjectOption | JSXOption

function valueToDisplay(option: VariableOption): string {
  switch (option.variableInfo.type) {
    case 'array':
      return `[]`
    case 'object':
      return `{}`
    case 'primitive':
      return `${option.variableInfo.value}`
    case 'jsx':
      return `JSX`
    default:
      assertNever(option.variableInfo)
  }
}

function isChildrenProp(path: PropertyPath): boolean {
  return (
    path.propertyElements.length > 0 &&
    typeof path.propertyElements[0] === 'string' &&
    path.propertyElements[0] === 'children'
  )
}

export interface DataPickerPopupProps {
  closePopup: () => void
  style: React.CSSProperties
  propPath: PropertyPath
}

export const DataPickerPopup = React.memo(
  React.forwardRef<HTMLDivElement, DataPickerPopupProps>((props, forwardedRef) => {
    const { closePopup, propPath } = props

    const selectedViewPathRef = useRefEditorState(
      (store) => store.editor.selectedViews.at(0) ?? null,
    )

    const colorTheme = useColorTheme()
    const dispatch = useDispatch()
    const isTargetingChildrenProp = isChildrenProp(propPath)

    const onTweakProperty = React.useCallback(
      (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => {
        if (selectedViewPathRef.current == null) {
          return
        }

        e.stopPropagation()
        e.preventDefault()

        const definedElseWhereArray = optionalMap((d) => [d], definedElsewhere) ?? []
        const expression = jsExpressionOtherJavaScriptSimple(name, definedElseWhereArray)

        if (isTargetingChildrenProp) {
          dispatch([
            {
              action: 'INSERT_ATTRIBUTE_OTHER_JAVASCRIPT',
              expression: expression,
              parent: selectedViewPathRef.current,
            },
          ])
          return
        }

        dispatch([setProp_UNSAFE(selectedViewPathRef.current, propPath, expression)])
      },
      [dispatch, isTargetingChildrenProp, propPath, selectedViewPathRef],
    )

    const variableNamesInScope = useVariablesInScopeForSelectedElement(
      selectedViewPathRef.current ?? EP.emptyElementPath,
      props.propPath,
    )

    return (
      <div
        style={{
          background: 'transparent',
          position: 'fixed',
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          zIndex: 1, // so it's above the inspector
        }}
        onClick={closePopup}
      >
        <FlexColumn
          ref={forwardedRef}
          tabIndex={0}
          style={{
            ...props.style,
            backgroundColor: colorTheme.neutralBackground.value,
            padding: '8px 16px',
            boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
            borderRadius: 8,
            alignItems: 'flex-start',
            width: '96%',
            maxWidth: '260px',
          }}
          data-testid={DataPickerPopupTestId}
        >
          <div style={{ fontSize: 11, fontWeight: 600, marginBottom: 16 }}>
            <span>Data</span>
          </div>
          {variableNamesInScope.map((variableOption, idx) => {
            return (
              <ValueRow
                key={variableOption.variableInfo.variableName}
                variableOption={variableOption}
                idx={`${idx}`}
                onTweakProperty={onTweakProperty}
              />
            )
          })}
        </FlexColumn>
      </div>
    )
  }),
)

interface ValueRowProps {
  variableOption: VariableOption
  idx: string
  onTweakProperty: (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => void
}

const anyObjectChildMatches = (info: VariableInfo): boolean =>
  info.type === 'object' && info.props.some((c) => c.matches || anyObjectChildMatches(c))

function ValueRow({ variableOption, idx, onTweakProperty }: ValueRowProps) {
  const colorTheme = useColorTheme()
  const [selectedIndex, setSelectedIndex] = React.useState<number>(0)

  const childrenLength = variableOption.type === 'array' ? variableOption.children.length : Infinity
  const childrenOpenByDefault =
    variableOption.depth < 2 ||
    childrenLength < 4 ||
    anyObjectChildMatches(variableOption.variableInfo)

  const [childrenOpen, setChildrenOpen] = React.useState<boolean>(childrenOpenByDefault)

  const toggleChildrenOpen = useCallback(() => {
    setChildrenOpen(!childrenOpen)
  }, [childrenOpen, setChildrenOpen])

  const isArray = variableOption.variableInfo.type === 'array'

  const tweakProperty = onTweakProperty(
    variableOption.variableInfo.variableName,
    variableOption.definedElsewhere,
  )
  const stopPropagation = useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  const variableChildren =
    variableOption.type === 'array' || variableOption.type === 'object'
      ? variableOption.children
      : null

  const hasObjectChildren = variableOption.type === 'object' && variableOption.children.length > 0
  return (
    <>
      <Button
        data-testid={VariableFromScopeOptionTestId(idx)}
        style={{
          width: '100%',
          height: 25,
          cursor: variableOption.variableInfo.matches ? 'pointer' : 'default',
        }}
        onClick={isArray ? stopPropagation : tweakProperty}
        css={{
          '&:hover': {
            backgroundColor: variableOption.variableInfo.matches
              ? colorTheme.secondaryBackground.value
              : 'inherit',
          },
        }}
      >
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{
            justifyContent: 'space-between',
            alignItems: 'flex-start',
            gap: 8,
            width: '100%',
            minHeight: 'auto',
            gridTemplateColumns: '48% 48%',
          }}
        >
          <div onClick={tweakProperty}>
            <span
              style={{
                marginLeft: 4 * variableOption.depth,
                borderRadius: 2,
                fontWeight: 400,
                display: 'flex',
                maxWidth: '100%',
              }}
            >
              <PrefixIcon
                depth={variableOption.depth}
                hasObjectChildren={hasObjectChildren}
                onIconClick={toggleChildrenOpen}
                open={childrenOpen}
              />
              <span
                data-testid='variable-name'
                style={{
                  textOverflow: 'ellipsis',
                  whiteSpace: 'nowrap',
                  overflow: 'hidden',
                  opacity: variableOption.variableInfo.matches ? 1 : 0.5,
                }}
              >
                {variableOption.variableInfo.displayName}
              </span>
            </span>
          </div>
          <div
            style={{
              display: 'flex',
              justifyContent: 'flex-end',
              width: '94%',
            }}
            onClick={isArray ? stopPropagation : tweakProperty}
          >
            <span
              style={{
                fontWeight: 400,
                color: colorTheme.neutralForeground.value,
                textOverflow: 'ellipsis',
                whiteSpace: 'nowrap',
                maxWidth: 130,
                overflow: 'hidden',
                opacity: variableOption.variableInfo.matches ? 1 : 0.5,
              }}
            >
              {isArray ? (
                <ArrayPaginator
                  selectedIndex={selectedIndex}
                  totalChildCount={childrenLength}
                  setSelectedIndex={setSelectedIndex}
                />
              ) : (
                valueToDisplay(variableOption)
              )}
            </span>
          </div>
        </UIGridRow>
      </Button>
      {variableChildren != null ? (
        isArray ? (
          <ValueRow
            key={variableChildren[selectedIndex].variableInfo.variableName}
            variableOption={variableChildren[selectedIndex]}
            idx={`${idx}-${selectedIndex}`}
            onTweakProperty={onTweakProperty}
          />
        ) : childrenOpen ? (
          variableChildren.map((child, index) => {
            return (
              <ValueRow
                key={child.variableInfo.variableName}
                variableOption={child}
                idx={`${idx}-${index}`}
                onTweakProperty={onTweakProperty}
              />
            )
          })
        ) : null
      ) : null}
    </>
  )
}

function PrefixIcon({
  depth,
  hasObjectChildren,
  onIconClick,
  open,
}: {
  depth: number
  hasObjectChildren: boolean
  onIconClick: () => void
  open: boolean
}) {
  const colorTheme = useColorTheme()
  const style = {
    width: 5,
    display: 'inline-block',
    height: 10,
    marginRight: 4,
    position: 'relative',
    top: 0,
    marginLeft: (depth - 1) * 8,
    flex: 'none',
  } as const
  const onClick = useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      onIconClick()
    },
    [onIconClick],
  )
  if (hasObjectChildren) {
    return (
      <span
        style={{ color: colorTheme.neutralBorder.value, fontSize: 6, ...style }}
        onClick={onClick}
      >
        {open ? '▼' : '▶'}
      </span>
    )
  }
  if (depth > 0) {
    return (
      <span
        style={{
          borderLeft: `1px solid ${colorTheme.neutralBorder.value}`,
          borderBottom: `1px solid ${colorTheme.neutralBorder.value}`,
          ...style,
        }}
      ></span>
    )
  }
  return null
}

function ArrayPaginator({
  selectedIndex,
  totalChildCount,
  setSelectedIndex,
}: {
  selectedIndex: number
  totalChildCount: number
  setSelectedIndex: (index: number) => void
}) {
  const increaseIndex = useCallback(() => {
    setSelectedIndex(Math.min(totalChildCount - 1, selectedIndex + 1))
  }, [selectedIndex, setSelectedIndex, totalChildCount])
  const decreaseIndex = useCallback(() => {
    setSelectedIndex(Math.max(0, selectedIndex - 1))
  }, [selectedIndex, setSelectedIndex])
  return (
    <span
      style={{
        fontSize: 9,
        color: 'gray',
      }}
    >
      <span onClick={decreaseIndex} style={{ width: 30, height: 30 }}>
        {'< '}
      </span>
      <span>
        {selectedIndex + 1} / {totalChildCount}
      </span>
      <span onClick={increaseIndex} style={{ width: 30, height: 30 }}>
        {' >'}
      </span>
    </span>
  )
}
