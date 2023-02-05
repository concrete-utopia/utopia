import React from 'react'
import { FlexRow, Icn, Icons, SimpleCSSNumberInput, SquareButton, UtopiaTheme } from '../../uuiui'
import { styled } from '@stitches/react'
import { usePopper } from 'react-popper'
import { CSSNumber, cssNumber, EmptyInputValue } from './common/css-utils'
import { NO_OP } from '../../core/shared/utils'
import {
  detectMinMaxDimension,
  MinMaxDimension,
} from './inspector-strategies/set-min-dimension-strategies'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  executeFirstApplicableStrategy,
  InspectorStrategy,
} from './inspector-strategies/inspector-strategy'
import {
  setMinMaxDimensionStrategies,
  unsetMinMaxDimensionStrategies,
} from './inspector-strategies/inspector-strategies'
import { createSelector } from 'reselect'

const AdvancedControlId = (dimension: MinMaxDimension) => `AdvancedControlId-${dimension}`
const RemoveContolTestId = (dimension: MinMaxDimension) => `AdvancedControlId-Remove-${dimension}`

const Container = styled('div', {
  display: 'flex',
  gap: 16,
  marginLeft: 8,
  padding: '4px 8px',
  width: 'max-content',
  cursor: 'pointer',
  backgroundColor: 'var(--utopitheme-fg9)',
  '&:hover': {
    backgroundColor: 'var(--utopitheme-fg8)',
  },
})

const PopupContainer = styled('div', {
  display: 'grid',
  gridTemplateColumns: '1fr 1fr',
  gap: 8,
  padding: 8,
  opacity: 1,
  width: '220px',
  background: 'var(--utopitheme-emphasizedBackground)',
  border: '1px solid var(--utopitheme-fg9)',
  boxShadow: '#0002 0px 0px 0px 1px, #0002 0px 4px 11px',
  zIndex: 1,
  borderRadius: UtopiaTheme.inputBorderRadius,
})

function strategyArrayForInputValue(
  value: number | EmptyInputValue,
  dimension: MinMaxDimension,
): Array<InspectorStrategy> {
  return typeof value === 'number'
    ? setMinMaxDimensionStrategies(dimension, cssNumber(value, null))
    : unsetMinMaxDimensionStrategies(dimension)
}

const MaxWidthSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectMinMaxDimension('max-width'),
)

const MinWidthSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectMinMaxDimension('min-width'),
)

const MaxHeightSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectMinMaxDimension('max-height'),
)

const MinHeightSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectMinMaxDimension('min-height'),
)

interface AdvancedControlProps {}

export const AdvancedControl = React.memo<AdvancedControlProps>(() => {
  const [referenceElement, setReferenceElement] = React.useState<HTMLDivElement | null>(null)
  const [popperElement, setPopperElement] = React.useState<HTMLDivElement | null>(null)
  const { styles, attributes } = usePopper(referenceElement, popperElement, {
    modifiers: [
      {
        name: 'offset',
        options: {
          offset: [0, 8],
        },
      },
    ],
  })

  const [popupIsOpen, setPopupIsOpen] = React.useState(false)
  const togglePopup = React.useCallback(() => setPopupIsOpen((v) => !v), [])

  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  // TODO: remove duplication here by mapping over all possible values of MinMaxDimension
  const unsetters: Record<MinMaxDimension, () => void> = React.useMemo(
    () => ({
      'max-height': () =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          unsetMinMaxDimensionStrategies('max-height'),
        ),
      'min-height': () =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          unsetMinMaxDimensionStrategies('min-height'),
        ),
      'max-width': () =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          unsetMinMaxDimensionStrategies('max-width'),
        ),
      'min-width': () =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          unsetMinMaxDimensionStrategies('min-width'),
        ),
    }),
    [dispatch, metadataRef, selectedViewsRef],
  )

  // TODO: remove duplication here by mapping over all possible values of MinMaxDimension
  const setters: Record<MinMaxDimension, (_: number | EmptyInputValue) => void> = React.useMemo(
    () => ({
      'max-height': (value) =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          strategyArrayForInputValue(value, 'max-height'),
        ),
      'min-height': (value) =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          strategyArrayForInputValue(value, 'min-height'),
        ),
      'max-width': (value) =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          strategyArrayForInputValue(value, 'max-width'),
        ),
      'min-width': (value) =>
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          strategyArrayForInputValue(value, 'min-width'),
        ),
    }),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const maxWidthValue = useEditorState(
    Substores.metadata,
    MaxWidthSelector,
    'AdvancedControl maxWidth',
  )

  const minWidthValue = useEditorState(
    Substores.metadata,
    MinWidthSelector,
    'AdvancedControl minWidth',
  )

  const maxHeightValue = useEditorState(
    Substores.metadata,
    MaxHeightSelector,
    'AdvancedControl maxHeight',
  )

  const minHeightValue = useEditorState(
    Substores.metadata,
    MinHeightSelector,
    'AdvancedControl minHeight',
  )

  return (
    <>
      <Container onClick={togglePopup} ref={setReferenceElement}>
        Advanced
        <Icn type='growToParent' color='main' category='layout/commands' width={18} height={18} />
      </Container>
      {popupIsOpen ? (
        <PopupContainer {...attributes.popper} style={styles.popper} ref={setPopperElement}>
          <FlexRow style={{ gap: 4 }}>
            <SquareButton
              highlight
              onMouseDown={unsetters['min-width']}
              data-testid={RemoveContolTestId('min-width')}
            >
              <Icons.Cross color={'secondary'} />
            </SquareButton>
            Min Width
          </FlexRow>
          <SimpleCSSNumberInput
            id={AdvancedControlId('min-width')}
            testId={AdvancedControlId('min-width')}
            value={minWidthValue}
            onSubmitValue={setters['min-width']}
            onTransientSubmitValue={NO_OP}
            onForcedSubmitValue={NO_OP}
            incrementControls={true}
            stepSize={1}
            minimum={0}
            maximum={Infinity}
            defaultUnitToHide={null}
            focusOnMount={false}
          />
          <FlexRow style={{ gap: 4 }}>
            <SquareButton
              highlight
              onMouseDown={unsetters['max-width']}
              data-testid={RemoveContolTestId('max-width')}
            >
              <Icons.Cross color={'secondary'} />
            </SquareButton>
            Max Width
          </FlexRow>
          <SimpleCSSNumberInput
            id={AdvancedControlId('max-width')}
            testId={AdvancedControlId('max-width')}
            value={maxWidthValue}
            onSubmitValue={setters['max-width']}
            onTransientSubmitValue={NO_OP}
            onForcedSubmitValue={NO_OP}
            incrementControls={true}
            stepSize={1}
            minimum={0}
            maximum={Infinity}
            defaultUnitToHide={null}
            focusOnMount={false}
          />
          <FlexRow style={{ gap: 4 }}>
            <SquareButton
              highlight
              onMouseDown={unsetters['min-height']}
              data-testid={RemoveContolTestId('min-height')}
            >
              <Icons.Cross color={'secondary'} />
            </SquareButton>
            Min Height
          </FlexRow>
          <SimpleCSSNumberInput
            id={AdvancedControlId('min-height')}
            testId={AdvancedControlId('min-height')}
            value={minHeightValue}
            onSubmitValue={setters['min-height']}
            onTransientSubmitValue={NO_OP}
            onForcedSubmitValue={NO_OP}
            incrementControls={true}
            stepSize={1}
            minimum={0}
            maximum={Infinity}
            defaultUnitToHide={null}
            focusOnMount={false}
          />
          <FlexRow style={{ gap: 4 }}>
            <SquareButton
              highlight
              onMouseDown={unsetters['max-height']}
              data-testid={RemoveContolTestId('max-height')}
            >
              <Icons.Cross color={'secondary'} />
            </SquareButton>
            Max Height
          </FlexRow>
          <SimpleCSSNumberInput
            id={AdvancedControlId('max-height')}
            testId={AdvancedControlId('max-height')}
            value={maxHeightValue}
            onSubmitValue={setters['max-height']}
            onTransientSubmitValue={NO_OP}
            onForcedSubmitValue={NO_OP}
            incrementControls={true}
            stepSize={1}
            minimum={0}
            maximum={Infinity}
            defaultUnitToHide={null}
            focusOnMount={false}
          />
        </PopupContainer>
      ) : null}
    </>
  )
})
