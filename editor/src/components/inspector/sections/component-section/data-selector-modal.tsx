import React from 'react'
import {
  FlexColumn,
  FlexRow,
  Icn,
  PopupList,
  UtopiaStyles,
  UtopiaTheme,
  useColorTheme,
} from '../../../../uuiui'
import type { VariableOption } from './data-picker-popup'
import { InspectorModal } from '../../widgets/inspector-modal'
import type { SelectOption } from '../../controls/select-control'
import { NO_OP } from '../../../../core/shared/utils'
import { getControlStyles } from '../../common/control-styles'

export interface DataSelectorModalProps {
  closePopup: () => void
  style: React.CSSProperties
  variablesInScope: VariableOption[]
}

const Separator = React.memo(
  ({
    color,
    spanGridColumns,
    margin,
  }: {
    color: string
    spanGridColumns?: number
    margin: number
  }) => {
    return (
      <div
        style={{
          width: '100%',
          height: 1,
          margin: `${margin}px 0px`,
          backgroundColor: color,
          gridColumn: spanGridColumns == null ? undefined : '1 / span 3',
        }}
      ></div>
    )
  },
)

function DataLabel({
  text,
  color,
  bgColor,
  icon,
  borderColor,
  borderRadius,
}: {
  text: string
  color?: string
  bgColor?: string
  borderRadius?: number
  borderColor?: string
  icon?: React.ReactElement
}) {
  return (
    <FlexRow
      style={{
        padding: '4px 8px',
        color: color,
        gap: 4,
        fontWeight: 600,
        alignItems: 'baseline',
        borderRadius: borderRadius,
        border: borderColor == null ? undefined : `1px solid ${borderColor}`,
        backgroundColor: bgColor == null ? undefined : bgColor,
        width: 'max-content',
        height: 'max-content',
        cursor: 'pointer',
      }}
    >
      {icon}
      <span>{text}</span>
    </FlexRow>
  )
}

const SIMPLE_CONTROL_STYLES = getControlStyles('simple')

function ElementSelector({
  total,
}: {
  total: number
  selected: number
  onSelect: (_: number) => void
}) {
  const options: SelectOption[] = React.useMemo(
    () =>
      Array(total)
        .fill(0)
        .map((_, i) => ({ value: i + 1, label: `${i + 1}` })),
    [total],
  )

  return (
    <PopupList
      id={'data-selector-index-select'}
      value={{ label: '1', value: 1 }}
      options={options}
      onSubmitValue={NO_OP}
      controlStyles={SIMPLE_CONTROL_STYLES}
      style={{ background: 'transparent' }}
    />
  )
}

const DEFAULT_SIZE: React.CSSProperties = { minWidth: 600, minHeight: 50, maxWidth: 700 }
const BRACES = '{}'
const CIRCLE = '◯'

export const DataSelectorModal = React.memo(
  React.forwardRef<HTMLDivElement, DataSelectorModalProps>(
    ({ style, closePopup, variablesInScope }, forwardedRef) => {
      const colorTheme = useColorTheme()

      //   {variablesInScope.map((v) => (
      //     <DataLabel
      //       key={v.valuePath.toString()}
      //       text={v.variableInfo.expression}
      //       bgColor={colorTheme.primary50.value}
      //       color={colorTheme.primary.value}
      //       borderRadius={4}
      //     />
      //   ))}

      return (
        <InspectorModal
          offsetX={20}
          offsetY={0}
          closePopup={closePopup}
          style={{
            zIndex: 1,
          }}
          closePopupOnUnmount={false}
          outsideClickIgnoreClass={'ignore-react-onclickoutside-data-picker'}
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
              style={{
                ...style,
                ...DEFAULT_SIZE,
                padding: 16,
                backgroundColor: colorTheme.inspectorBackground.value,
                color: colorTheme.fg1.value,
                overflow: 'hidden',
                borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
                boxShadow: UtopiaStyles.shadowStyles.highest.boxShadow,
                border: `1px solid ${colorTheme.fg0Opacity10.value}`,
              }}
            >
              {/* top bar */}
              <FlexRow style={{ gap: 8 }}>
                <Icn
                  category='semantic'
                  type='icon-semantic-back'
                  width={18}
                  height={18}
                  style={{ cursor: 'pointer' }}
                />
                <DataLabel
                  text='sample_data'
                  borderRadius={4}
                  borderColor={colorTheme.fg0Opacity20.value}
                  icon={
                    <span
                      style={{
                        color: colorTheme.brandNeonGreen.value,
                        fontWeight: 700,
                        fontSize: 14,
                      }}
                    >
                      {BRACES}
                    </span>
                  }
                />
              </FlexRow>
              <Separator color={colorTheme.seperator.value} margin={12} />
              {/* detail view */}
              <FlexColumn>
                <div style={{ display: 'grid', gridTemplateColumns: 'auto 40px 1fr', gap: 8 }}>
                  <DataLabel
                    text='testimonials'
                    bgColor={colorTheme.primary50.value}
                    color={colorTheme.primary.value}
                    borderRadius={4}
                  />
                  <ElementSelector total={4} selected={1} onSelect={NO_OP} />
                  {/* properties in scope */}
                  <FlexRow style={{ flexWrap: 'wrap', height: 'max-content' }}>
                    {Array(12)
                      .fill(0)
                      .map((_, i) => (
                        <DataLabel
                          key={i}
                          text='username'
                          icon={
                            <span
                              style={{
                                color: colorTheme.brandNeonGreen.value,
                                fontWeight: 700,
                                fontSize: 10,
                              }}
                            >
                              {CIRCLE}
                            </span>
                          }
                        />
                      ))}
                  </FlexRow>
                  <Separator color={colorTheme.seperator.value} spanGridColumns={3} margin={4} />
                  <DataLabel
                    text='productFeatures'
                    bgColor={colorTheme.brandNeonPink60.value}
                    color={colorTheme.brandNeonPink.value}
                    borderRadius={4}
                  />
                  <div />
                  {/* properties in scope */}
                  <FlexRow style={{ flexWrap: 'wrap', height: 'max-content' }}>
                    {Array(8)
                      .fill(0)
                      .map((_, i) => (
                        <DataLabel
                          key={i}
                          text='username'
                          icon={
                            <span
                              style={{
                                color: colorTheme.brandNeonGreen.value,
                                fontWeight: 700,
                                fontSize: 10,
                              }}
                            >
                              {CIRCLE}
                            </span>
                          }
                        />
                      ))}
                  </FlexRow>
                  <Separator color={colorTheme.seperator.value} spanGridColumns={3} margin={4} />
                  <DataLabel text='productFeatures' />
                  <div />
                  {/* properties in scope */}
                  <FlexRow style={{ flexWrap: 'wrap', height: 'max-content' }}>
                    {Array(2)
                      .fill(0)
                      .map((_, i) => (
                        <DataLabel
                          key={i}
                          text='username'
                          icon={
                            <span
                              style={{
                                color: colorTheme.brandNeonGreen.value,
                                fontWeight: 700,
                                fontSize: 10,
                              }}
                            >
                              {CIRCLE}
                            </span>
                          }
                        />
                      ))}
                  </FlexRow>
                </div>
              </FlexColumn>
            </FlexColumn>
          </div>
        </InspectorModal>
      )
    },
  ),
)
