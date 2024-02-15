import React from 'react'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { PropertyPath } from '../../../../core/shared/project-file-types'
import { useColorTheme, UtopiaTheme, Button, FlexColumn, UtopiaStyles } from '../../../../uuiui'
import { setProp_UNSAFE } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { DataPickerPopupTestId, VariableFromScopeOptionTestId } from './component-section'
import * as EP from '../../../../core/shared/element-path'
import { useVariablesInScopeForSelectedElement } from './variables-in-scope-utils'

export interface VariableOption {
  variableName: string
  displayName: string
  definedElsewhere: string | null
  value: string
  depth: number
  variableChildren?: Array<VariableOption>
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

    const onTweakProperty = React.useCallback(
      (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => {
        if (selectedViewPathRef.current == null) {
          return
        }

        e.stopPropagation()
        e.preventDefault()

        const definedElseWhereArray = optionalMap((d) => [d], definedElsewhere) ?? []

        dispatch([
          setProp_UNSAFE(
            selectedViewPathRef.current,
            propPath,
            jsExpressionOtherJavaScriptSimple(name, definedElseWhereArray),
          ),
        ])
      },
      [dispatch, propPath, selectedViewPathRef],
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
            borderRadius: UtopiaTheme.inputBorderRadius,
            alignItems: 'flex-start',
            width: '96%',
          }}
          data-testid={DataPickerPopupTestId}
        >
          <div style={{ fontSize: 14, fontWeight: 400, marginBottom: 16 }}>
            <span>Data</span>
          </div>
          {variableNamesInScope.map((variableOption, idx) => {
            return (
              <ValueRow
                key={variableOption.variableName}
                variableOption={variableOption}
                idx={idx}
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
  idx: number
  onTweakProperty: (name: string, definedElsewhere: string | null) => (e: React.MouseEvent) => void
}

function ValueRow({ variableOption, idx, onTweakProperty }: ValueRowProps) {
  const colorTheme = useColorTheme()

  const {
    variableName,
    definedElsewhere,
    value,
    displayName,
    depth = 0,
    variableChildren,
  } = variableOption
  return (
    <>
      <Button
        data-testid={VariableFromScopeOptionTestId(idx)}
        key={variableName}
        onClick={onTweakProperty(variableName, definedElsewhere)}
        style={{ width: '100%', height: 25 }}
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
          <div>
            <span
              style={{
                marginLeft: 4 * depth,
                borderRadius: 2,
                fontWeight: 400,
                display: 'flex',
                maxWidth: '100%',
              }}
            >
              {depth > 0 ? (
                <span
                  style={{
                    borderLeft: `1px solid ${colorTheme.neutralBorder.value}`,
                    borderBottom: `1px solid ${colorTheme.neutralBorder.value}`,
                    width: 5,
                    display: 'inline-block',
                    height: 10,
                    marginRight: 4,
                    position: 'relative',
                    top: 0,
                    marginLeft: (depth - 1) * 8,
                    flex: 'none',
                    textOverflow: 'ellipsis',
                    overflow: 'hidden',
                  }}
                ></span>
              ) : null}
              <span
                style={{
                  textOverflow: 'ellipsis',
                  overflow: 'hidden',
                }}
              >
                {displayName}
              </span>
            </span>
          </div>
          <div
            style={{
              display: 'flex',
              justifyContent: 'flex-end',
              width: '94%',
            }}
          >
            <span
              style={{
                fontWeight: 400,
                color: colorTheme.neutralForeground.value,
                textOverflow: 'ellipsis',
                maxWidth: 130,
                overflow: 'hidden',
              }}
            >
              {value}
            </span>
          </div>
        </UIGridRow>
      </Button>
      {variableChildren != null
        ? variableChildren.map((child, index) => {
            return (
              <ValueRow
                key={child.variableName}
                variableOption={child}
                idx={index}
                onTweakProperty={onTweakProperty}
              />
            )
          })
        : null}
    </>
  )
}
