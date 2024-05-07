/** @jsxRuntime classic */
/** @jsxFrag */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useCallback } from 'react'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import { useColorTheme, Button, FlexColumn, UtopiaStyles } from '../../../../uuiui'
import {
  insertJSXElement,
  setProp_UNSAFE,
  updateMapExpression,
} from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { DataPickerPopupTestId, VariableFromScopeOptionTestId } from './component-section'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import type {
  ArrayInfo,
  JSXInfo,
  ObjectInfo,
  PrimitiveInfo,
  VariableInfo,
} from './variables-in-scope-utils'
import { useVariablesInScopeForSelectedElement } from './variables-in-scope-utils'
import { NO_OP, arrayEqualsByValue, assertNever } from '../../../../core/shared/utils'
import { isPrefixOf } from '../../../../core/shared/array-utils'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import { FlexRow } from 'utopia-api'
import { is } from '../../../../core/shared/equality-utils'
import { atom, useAtom } from 'jotai'
import type { SelectOption } from '../../controls/select-control'
import { InspectorModal } from '../../widgets/inspector-modal'
import { emptyImports } from '../../../../core/workers/common/project-file-utils'

export interface PrimitiveOption {
  type: 'primitive'
  variableInfo: PrimitiveInfo
  definedElsewhere: string
  depth: number
  valuePath: Array<string | number>
  disabled: boolean
}

export interface ArrayOption {
  type: 'array'
  variableInfo: ArrayInfo
  depth: number
  definedElsewhere: string
  children: Array<VariableOption>
  valuePath: Array<string | number>
  disabled: boolean
}

export interface ObjectOption {
  type: 'object'
  variableInfo: ObjectInfo
  depth: number
  definedElsewhere: string
  children: Array<VariableOption>
  valuePath: Array<string | number>
  disabled: boolean
}

export interface JSXOption {
  type: 'jsx'
  variableInfo: JSXInfo
  definedElsewhere: string
  depth: number
  valuePath: Array<string | number>
  disabled: boolean
}

export type VariableOption = PrimitiveOption | ArrayOption | ObjectOption | JSXOption

const DataPickerFilterOptions = ['all', 'preferred'] as const
export type DataPickerFilterOption = (typeof DataPickerFilterOptions)[number]
export function dataPickerFilterOptionToString(mode: DataPickerFilterOption): string {
  switch (mode) {
    case 'all':
      return 'All Data'
    case 'preferred':
      return 'Preferred'
    default:
      assertNever(mode)
  }
}

export const DataPickerPreferredAllAtom = atom<DataPickerFilterOption>('preferred')

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
  pickerType: DataPickerType
}

export interface DataPickerForAProperty {
  type: 'FOR_A_PROPERTY'
  elementPath: ElementPath
  propPath: PropertyPath
  propExpressionPath: Array<string | number> | null
}

export function dataPickerForAProperty(
  elementPath: ElementPath,
  propPath: PropertyPath,
  propExpressionPath: Array<string | number> | null,
): DataPickerForAProperty {
  return {
    type: 'FOR_A_PROPERTY',
    elementPath: elementPath,
    propPath: propPath,
    propExpressionPath: propExpressionPath,
  }
}

export interface DataPickerForAMapExpressionValue {
  type: 'FOR_A_MAP_EXPRESSION_VALUE'
  elementPath: ElementPath
}

export function dataPickerForAnElement(elementPath: ElementPath): DataPickerForAMapExpressionValue {
  return {
    type: 'FOR_A_MAP_EXPRESSION_VALUE',
    elementPath: elementPath,
  }
}

export type DataPickerType = DataPickerForAProperty | DataPickerForAMapExpressionValue

export function dataPickerIgnoreClass(pickerType: DataPickerType): string {
  switch (pickerType.type) {
    case 'FOR_A_PROPERTY':
      return `ignore-react-onclickoutside-data-picker-${PP.toString(pickerType.propPath)}`
    case 'FOR_A_MAP_EXPRESSION_VALUE':
      return `ignore-react-onclickoutside-data-picker-${EP.toString(pickerType.elementPath)}`
    default:
      assertNever(pickerType)
  }
}

export interface DataPickerPopupSubvariablesProps {
  preferredAllState: DataPickerFilterOption
  pickerType: DataPickerType
  onTweakProperty: (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => void
  customizeOptions: (_: VariableOption[]) => VariableOption[]
}

export const DataPickerPopupSubvariables = React.memo((props: DataPickerPopupSubvariablesProps) => {
  const { pickerType, preferredAllState, onTweakProperty } = props
  const variableNamesInScope = useVariablesInScopeForSelectedElement(
    props.pickerType.elementPath,
    props.pickerType.type === 'FOR_A_PROPERTY' ? props.pickerType.propPath : null,
    preferredAllState,
  )

  const filteredVariableNamesInScope = React.useMemo(
    () => props.customizeOptions(variableNamesInScope),
    [props, variableNamesInScope],
  )

  return (
    <>
      {filteredVariableNamesInScope.map((variableOption, idx) => {
        return (
          <ValueRow
            key={variableOption.valuePath.toString()}
            variableOption={variableOption}
            idx={`${idx}`}
            onTweakProperty={onTweakProperty}
            pickerType={pickerType}
          />
        )
      })}
    </>
  )
})

export interface DataPickerPopupProps {
  closePopup: () => void
  customizeOptions: (_: VariableOption[]) => VariableOption[]
  style: React.CSSProperties
  pickerType: DataPickerType
}

export const DataPickerPopup = React.memo(
  React.forwardRef<HTMLDivElement, DataPickerPopupProps>((props, forwardedRef) => {
    const { closePopup, pickerType } = props

    const [preferredAllState, setPreferredAllState] = useAtom(DataPickerPreferredAllAtom)

    const colorTheme = useColorTheme()
    const dispatch = useDispatch()

    const setMode = React.useCallback(
      (option: SelectOption<DataPickerFilterOption>) => {
        setPreferredAllState(option.value)
      },
      [setPreferredAllState],
    )

    const onTweakProperty = React.useCallback(
      (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()

        const definedElseWhereArray = optionalMap((d) => [d], definedElsewhere) ?? []
        const expression = jsExpressionOtherJavaScriptSimple(name, definedElseWhereArray)

        switch (pickerType.type) {
          case 'FOR_A_PROPERTY':
            const isTargetingChildrenProp = isChildrenProp(pickerType.propPath)
            if (isTargetingChildrenProp) {
              dispatch([
                {
                  action: 'INSERT_ATTRIBUTE_OTHER_JAVASCRIPT_INTO_ELEMENT',
                  expression: expression,
                  parent: props.pickerType.elementPath,
                },
              ])
              return
            }

            dispatch([
              setProp_UNSAFE(props.pickerType.elementPath, pickerType.propPath, expression),
            ])
            break
          case 'FOR_A_MAP_EXPRESSION_VALUE':
            dispatch([updateMapExpression(props.pickerType.elementPath, expression)])
            break
          default:
            assertNever(pickerType)
        }
      },
      [dispatch, pickerType, props.pickerType.elementPath],
    )

    const filterOptions = React.useMemo(
      () =>
        DataPickerFilterOptions.map((option) => ({
          value: option,
          label: dataPickerFilterOptionToString(option),
        })),
      [],
    )

    const handleFilterOptionClick = useCallback(
      (option: { value: any }) => (e: React.MouseEvent) => {
        e.stopPropagation()
        setMode({ value: option.value })
      },
      [setMode],
    )

    return (
      <InspectorModal
        offsetX={10}
        offsetY={0}
        closePopup={props.closePopup}
        style={{
          zIndex: 1,
        }}
        closePopupOnUnmount={false}
        outsideClickIgnoreClass={dataPickerIgnoreClass(pickerType)}
      >
        <div // this entire wrapper div was made before using the InspectorModal, so it should be re-done
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
              left: -16, // to make it align with the cartouche control
              backgroundColor: colorTheme.contextMenuBackground.value,
              color: colorTheme.contextMenuForeground.value,
              padding: 8,
              boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
              borderRadius: 10,
              alignItems: 'flex-start',
              width: '96%',
              maxWidth: '260px',
              maxHeight: '260px',
            }}
            data-testid={DataPickerPopupTestId}
          >
            <div
              css={{
                overflowX: 'scroll',
                whiteSpace: 'nowrap',
                display: 'flex',
                flexDirection: 'row',
                gap: 16,
                padding: '2px',
                flex: 'none',
                paddingBottom: '6px',
              }}
            >
              {filterOptions.map((option, index) => (
                <div
                  key={index}
                  css={{
                    color: colorTheme.white.value,
                    fontWeight: 500,
                    opacity: preferredAllState === option.value ? 1 : 0.4,
                    '&:hover': {
                      opacity: 1,
                    },
                  }}
                  onClick={handleFilterOptionClick(option)}
                >
                  {option.label}
                </div>
              ))}
            </div>
            <div
              style={{
                overflowY: 'auto',
                height: '100%',
                width: '100%',
                scrollbarWidth: 'auto',
                colorScheme: 'dark',
                scrollbarColor: 'gray transparent',
              }}
            >
              <DataPickerPopupSubvariables
                preferredAllState={preferredAllState}
                pickerType={pickerType}
                onTweakProperty={onTweakProperty}
                customizeOptions={props.customizeOptions}
              />
            </div>
          </FlexColumn>
        </div>
      </InspectorModal>
    )
  }),
)

interface ValueRowProps {
  variableOption: VariableOption
  idx: string
  onTweakProperty: (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => void
  pickerType: DataPickerType
  overriddenTitle?: string
}

const anyObjectChildMatches = (info: VariableInfo): boolean =>
  info.type === 'object' && info.props.some((c) => c.matches || anyObjectChildMatches(c))

function ValueRow({
  variableOption,
  idx,
  onTweakProperty,
  pickerType,
  overriddenTitle,
}: ValueRowProps) {
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
    variableOption.variableInfo.expression,
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

  const currentExpressionExactMatch =
    pickerType.type === 'FOR_A_PROPERTY' &&
    pickerType.propExpressionPath != null &&
    arrayEqualsByValue(variableOption.valuePath, pickerType.propExpressionPath, is)

  const onClickTopLevelButton = React.useCallback(
    (e: React.MouseEvent) => {
      if (variableOption.disabled) {
        return
      }
      if (isArray) {
        return stopPropagation(e)
      }
      return tweakProperty(e)
    },
    [isArray, stopPropagation, tweakProperty, variableOption.disabled],
  )

  const onClickVariable = React.useCallback(
    (e: React.MouseEvent) => {
      if (variableOption.disabled) {
        return
      }

      return tweakProperty(e)
    },
    [tweakProperty, variableOption.disabled],
  )

  return (
    <>
      <Button
        data-testid={VariableFromScopeOptionTestId(idx)}
        style={{
          borderRadius: 4,
          width: '100%',
          height: 28,
          marginTop: variableChildren != null && variableOption.depth === 0 ? 6 : 0, // add some space between top-level variables
          cursor: variableOption.variableInfo.matches ? 'pointer' : 'default',
          background: currentExpressionExactMatch ? colorTheme.primary.value : undefined,
          color: currentExpressionExactMatch ? colorTheme.white.value : undefined,
          paddingLeft: variableOption.depth * 8,
        }}
        onClick={onClickTopLevelButton}
        css={{
          '&:hover': {
            backgroundColor: colorTheme.primary30.value,
          },
        }}
      >
        <UIGridRow
          padded
          variant='<--1fr--><--1fr-->'
          style={{
            justifyContent: 'space-between',
            alignItems: 'flex-start',
            gap: 10,
            width: '100%',
            minHeight: 'auto',
            gridTemplateColumns: '48% 48%',
          }}
        >
          <div onClick={onClickVariable} data-label='left column cell'>
            <div
              data-testid={`variable-from-scope-span-${variableOption.valuePath}`}
              style={{ display: 'grid', gridTemplateColumns: '16px 1fr' }}
            >
              <PrefixIcon
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
                  opacity:
                    variableOption.variableInfo.matches && !variableOption.disabled ? 1 : 0.5,
                }}
              >
                {overriddenTitle ?? variableOption.variableInfo.expressionPathPart}
              </span>
            </div>
          </div>

          {variableChildren == null || variableChildren.length === 0 ? null : (
            <div
              data-label='right-column cell'
              style={{
                display: 'flex',
                width: '94%',
              }}
              onClick={isArray ? stopPropagation : tweakProperty}
            >
              <span
                style={{
                  fontWeight: 400,
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
                  <div style={{ opacity: 0.3 }}>{valueToDisplay(variableOption)}</div>
                )}
              </span>
            </div>
          )}
        </UIGridRow>
      </Button>
      {variableChildren != null && variableChildren.at(selectedIndex) != null ? (
        isArray ? (
          <ValueRow
            variableOption={variableChildren[selectedIndex]}
            idx={`${idx}-${selectedIndex}`}
            onTweakProperty={onTweakProperty}
            pickerType={pickerType}
            overriddenTitle={`${variableOption.variableInfo.expressionPathPart}[${selectedIndex}]`}
          />
        ) : childrenOpen ? (
          variableChildren.map((child, index) => {
            return (
              <ValueRow
                key={child.valuePath.toString()}
                variableOption={child}
                idx={`${idx}-${index}`}
                onTweakProperty={onTweakProperty}
                pickerType={pickerType}
              />
            )
          })
        ) : null
      ) : null}
    </>
  )
}

function PrefixIcon({
  hasObjectChildren,
  onIconClick,
  open,
}: {
  hasObjectChildren: boolean
  onIconClick: () => void
  open: boolean
}) {
  const colorTheme = useColorTheme()
  const style = {
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
  } as const
  const onClick = useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      onIconClick()
    },
    [onIconClick],
  )

  return (
    <span
      css={{
        color: colorTheme.neutralBorder.value,
        fontSize: 6,
        ...style,
      }}
      onClick={onClick}
    >
      {hasObjectChildren ? (
        <ExpandableIndicator visible collapsed={!open} selected={false} iconColor='white' />
      ) : null}
    </span>
  )
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
    <FlexRow
      css={{
        alignItems: 'center',
        fontSize: 10,
      }}
    >
      <div onClick={decreaseIndex} style={{ cursor: 'pointer', paddingLeft: 4, paddingRight: 4 }}>
        {'< '}
      </div>
      <span>
        {selectedIndex + 1} / {totalChildCount}
      </span>
      <span onClick={increaseIndex} style={{ cursor: 'pointer', paddingLeft: 4, paddingRight: 4 }}>
        {' >'}
      </span>
    </FlexRow>
  )
}
