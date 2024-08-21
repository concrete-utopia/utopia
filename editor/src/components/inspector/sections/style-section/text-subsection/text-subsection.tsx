import * as OPI from 'object-path-immutable'
import React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import {
  emptyComments,
  isJSXAttributeValue,
  JSExpression,
  jsExpressionValue,
} from '../../../../../core/shared/element-template'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { useKeepShallowReferenceEquality } from '../../../../../utils/react-performance'
import utils from '../../../../../utils/utils'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  InspectorSubsectionHeader,
  Icons,
  NumberInput,
  SquareButton,
  colorTheme,
  UtopiaTheme,
} from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { EditorAction } from '../../../../editor/action-types'
import * as EditorActions from '../../../../editor/actions/action-creators'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import type { CSSFontStyle, CSSTextDecorationLine } from '../../../common/css-utils'
import { cssNumber, ParsedCSSPropertiesKeys } from '../../../common/css-utils'
import {
  RemovePropertyButton,
  usePropControlledRef_DANGEROUS,
} from '../../../common/inspector-utils'
import {
  InspectorCallbackContext,
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorContext,
  useInspectorElementInfo,
  useInspectorStyleInfo,
  useIsSubSectionVisible,
  useSelectedViews,
} from '../../../common/property-path-hooks'
import { ColorControl } from '../../../controls/color-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { OptionControl } from '../../../controls/option-control'
import { PropertyRow } from '../../../widgets/property-row'
import { FontFamilySelect } from './font-family-select'
import { FontVariantSelect } from './font-variant-select'
import { FlexRow } from 'utopia-api'
import { TextRelatedProperties } from '../../../../../core/properties/css-properties'
import { useContextSelector } from 'use-context-selector'
import { TextAutoSizingControl } from './text-auto-sizing-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { TextAlignControl } from './text-align-control'

const ObjectPathImmutable: any = OPI

function updateItalicFontStyle(newValue: boolean, oldValue: CSSFontStyle): CSSFontStyle {
  return newValue ? 'italic' : 'normal'
}

const spaceRegex = /\s+/

function updateTextDecorationLine(
  addValue: boolean,
  toAddOrRemove: string,
  currentValue: CSSTextDecorationLine,
): CSSTextDecorationLine {
  if (currentValue === 'none') {
    if (addValue) {
      return toAddOrRemove
    } else {
      return 'none'
    }
  } else {
    if (addValue) {
      if (currentValue.includes(toAddOrRemove)) {
        return currentValue
      } else {
        return `${currentValue} ${toAddOrRemove}`
      }
    } else {
      const possibleNewValue = currentValue
        .split(spaceRegex)
        .filter((value) => value !== toAddOrRemove)
        .join(' ')
        .trim()
      return possibleNewValue === '' ? 'none' : possibleNewValue
    }
  }
}

export function updateUnderlinedTextDecoration(
  newValue: boolean,
  oldValue: CSSTextDecorationLine,
): CSSTextDecorationLine {
  return updateTextDecorationLine(newValue, 'underline', oldValue)
}

export function updateStrikethroughTextDecoration(
  newValue: boolean,
  oldValue: CSSTextDecorationLine,
): CSSTextDecorationLine {
  return updateTextDecorationLine(newValue, 'line-through', oldValue)
}

const normalLetterSpacingAsCSSNumber = cssNumber(0, 'px')
const normalLineHeightAsCSSNumber = cssNumber(0, 'px')

export const TextSubsection = React.memo(() => {
  const [expanded, setExpanded] = React.useState(false)

  const colorMetadata = useInspectorStyleInfo('color')

  const fontFamilyMetadata = useInspectorStyleInfo('fontFamily')

  const fontStyleMetadata = useInspectorStyleInfo('fontStyle')

  const [onItalicSubmitValue] = fontStyleMetadata.useSubmitValueFactory(updateItalicFontStyle)

  const fontSizeMetadata = useInspectorStyleInfo('fontSize')

  const textAlignMetadata = useInspectorStyleInfo('textAlign')

  const textDecorationLineMetadata = useInspectorStyleInfo('textDecorationLine')

  const [onUnderlinedSubmitValue] = textDecorationLineMetadata.useSubmitValueFactory(
    updateUnderlinedTextDecoration,
  )

  const [onStrikethroughSubmitValue] = textDecorationLineMetadata.useSubmitValueFactory(
    updateStrikethroughTextDecoration,
  )

  const letterSpacingMetadata = useInspectorStyleInfo('letterSpacing')

  const lineHeightMetadata = useInspectorStyleInfo('lineHeight')

  const isVisible = useIsSubSectionVisible('text')

  const showSubsectionUnsetContextMenuItem =
    colorMetadata.controlStyles.unsettable ||
    fontFamilyMetadata.controlStyles.unsettable ||
    fontStyleMetadata.controlStyles.unsettable ||
    fontSizeMetadata.controlStyles.unsettable ||
    textAlignMetadata.controlStyles.unsettable ||
    textDecorationLineMetadata.controlStyles.unsettable ||
    letterSpacingMetadata.controlStyles.unsettable ||
    lineHeightMetadata.controlStyles.unsettable

  const anyTextRelatedPropSet = [
    colorMetadata,
    fontFamilyMetadata,
    fontStyleMetadata,
    fontSizeMetadata,
    textAlignMetadata,
    textDecorationLineMetadata,
    letterSpacingMetadata,
    lineHeightMetadata,
  ].some((m) => m.propertyStatus.set)

  const { onContextUnsetValue } = useInspectorContext()

  const targetPath = useContextSelector(InspectorPropsContext, (context) => context.targetPath)

  const onUnsetSubsectionValues = React.useCallback(() => {
    onContextUnsetValue(
      TextRelatedProperties.map((prop) => {
        return PP.createFromArray([...targetPath, prop])
      }),
      false,
    )
  }, [onContextUnsetValue, targetPath])

  const subsectionContextMenuItems = utils.stripNulls([
    showSubsectionUnsetContextMenuItem
      ? addOnUnsetValues(['all font properties'], onUnsetSubsectionValues)
      : null,
  ])

  const fontSizeContextMenuItems = utils.stripNulls([
    fontSizeMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['fontSize'], fontSizeMetadata.onUnsetValues)
      : null,
  ])

  const colorContextMenuItems = utils.stripNulls([
    colorMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['color'], colorMetadata.onUnsetValues)
      : null,
  ])

  const textAlignContextMenuItems = utils.stripNulls([
    textAlignMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['textAlign'], textAlignMetadata.onUnsetValues)
      : null,
  ])

  const italicContextMenuItems = utils.stripNulls([
    fontStyleMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['fontStyle'], fontStyleMetadata.onUnsetValues)
      : null,
  ])

  const underlineContextMenuItems = utils.stripNulls([
    textDecorationLineMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['textDecorationLine'], textDecorationLineMetadata.onUnsetValues)
      : null,
  ])

  const strikethroughContextMenuItems = utils.stripNulls([
    textDecorationLineMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['textDecorationLine'], textDecorationLineMetadata.onUnsetValues)
      : null,
  ])

  const letterSpacingContextMenuItems = utils.stripNulls([
    letterSpacingMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['letterSpacing'], letterSpacingMetadata.onUnsetValues)
      : null,
  ])

  const lineHeightContextMenuItems = utils.stripNulls([
    lineHeightMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['lineHeight'], lineHeightMetadata.onUnsetValues)
      : null,
  ])

  const toggleExpanded = React.useCallback(
    () => setExpanded((prevExpanded) => !prevExpanded),
    [setExpanded],
  )

  const wrappedFontSizeOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    fontSizeMetadata.onSubmitValue,
    fontSizeMetadata.onUnsetValues,
  )
  const wrappedFontSizeOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    fontSizeMetadata.onTransientSubmitValue,
    fontSizeMetadata.onUnsetValues,
  )

  const wrappedLetterSpacingOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    letterSpacingMetadata.onSubmitValue,
    letterSpacingMetadata.onUnsetValues,
  )
  const wrappedLetterSpacingOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    letterSpacingMetadata.onTransientSubmitValue,
    letterSpacingMetadata.onUnsetValues,
  )

  const wrappedLineHeightOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    lineHeightMetadata.onSubmitValue,
    lineHeightMetadata.onUnsetValues,
  )
  const wrappedLineHeightOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    lineHeightMetadata.onTransientSubmitValue,
    lineHeightMetadata.onUnsetValues,
  )

  if (!isVisible) {
    return null
  }

  const isUnderlined = textDecorationLineMetadata.value.includes('underline')
  const isStruckthrough = textDecorationLineMetadata.value.includes('line-through')

  return (
    <div style={{ paddingBottom: 10 }}>
      <InspectorContextMenuWrapper
        id='text-subsection-context-menu'
        items={subsectionContextMenuItems}
        data={null}
      >
        <InspectorSubsectionHeader>
          <FlexRow
            css={{
              flexGrow: 1,
              gap: 8,
            }}
          >
            <span>Type</span>
          </FlexRow>
          <RemovePropertyButton
            testId='inspector-text-remove-all'
            onUnsetValues={onUnsetSubsectionValues}
            propertySet={anyTextRelatedPropSet}
          />
          <SquareButton highlight onClick={toggleExpanded} spotlight={expanded ? true : false}>
            <Icons.Threedots />
          </SquareButton>
        </InspectorSubsectionHeader>
      </InspectorContextMenuWrapper>
      <FontFamilySelect />
      <UIGridRow
        padded={false}
        variant='<--------1fr-------->|60px||28px|'
        style={{
          padding: '0 8px 0 4px',
          alignItems: 'center',
          minHeight: UtopiaTheme.layout.rowHeight.normal,
        }}
      >
        <FontVariantSelect />
        <NumberInput
          id='fontSize'
          testId='fontSize'
          key='font-size-number-input'
          value={fontSizeMetadata.value}
          controlStatus={fontSizeMetadata.controlStatus}
          onSubmitValue={wrappedFontSizeOnSubmitValue}
          onTransientSubmitValue={wrappedFontSizeOnTransientSubmitValue}
          minimum={0}
          numberType='Length'
          defaultUnitToHide={'px'}
          incrementControls
        />
        <ColorControl
          id='color-control'
          key='color-control'
          testId='text-subsection-color-control'
          value={colorMetadata.value}
          onSubmitValue={colorMetadata.onSubmitValue}
          onTransientSubmitValue={colorMetadata.onTransientSubmitValue}
          pickerOffset={{ x: -223, y: 0 }}
          controlStatus={colorMetadata.controlStatus}
          controlStyles={colorMetadata.controlStyles}
        />
      </UIGridRow>
      <FlexRow
        style={{
          gridColumnGap: 8,
          padding: '0 8px',
          minHeight: UtopiaTheme.layout.rowHeight.normal,
          alignItems: 'center',
        }}
        css={undefined}
      >
        <TextAutoSizingControl />
        <TextAlignControl />
      </FlexRow>
      {expanded ? (
        <FlexRow
          css={{
            padding: '0 8px',
            minHeight: UtopiaTheme.layout.rowHeight.normal,
            alignItems: 'center',
            gap: 8,
          }}
        >
          <FlexRow css={{ gap: 4, flex: 1 }}>
            <OptionControl
              id='italic'
              key='italic'
              testId='italic'
              value={fontStyleMetadata.value === 'italic'}
              onSubmitValue={onItalicSubmitValue}
              controlStatus={fontStyleMetadata.controlStatus}
              controlStyles={fontStyleMetadata.controlStyles}
              style={{ border: `1px solid ${colorTheme.bg2.value}` }}
              DEPRECATED_controlOptions={{
                tooltip: 'Italic',
                icon: {
                  category: 'typography',
                  type: 'italic',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              }}
            />
            <OptionControl
              id='underlined'
              key='underlined'
              testId='underlined'
              value={isUnderlined}
              onSubmitValue={onUnderlinedSubmitValue}
              controlStatus={textDecorationLineMetadata.controlStatus}
              controlStyles={textDecorationLineMetadata.controlStyles}
              style={{ border: `1px solid ${colorTheme.bg2.value}` }}
              DEPRECATED_controlOptions={{
                tooltip: 'Underline',
                icon: {
                  category: 'typography',
                  type: 'underline',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              }}
            />
            <OptionControl
              id='strikethrough'
              key='strikethrough'
              testId='strikethrough'
              value={isStruckthrough}
              onSubmitValue={onStrikethroughSubmitValue}
              controlStatus={textDecorationLineMetadata.controlStatus}
              controlStyles={textDecorationLineMetadata.controlStyles}
              style={{ border: `1px solid ${colorTheme.bg2.value}` }}
              DEPRECATED_controlOptions={{
                tooltip: 'Strikethrough',
                icon: {
                  category: 'inspector-element',
                  type: 'strikethrough',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              }}
            />
          </FlexRow>
          <NumberInput
            key='letterSpacing'
            id='letterSpacing'
            testId='letterSpacing'
            value={
              letterSpacingMetadata.value === 'normal'
                ? normalLetterSpacingAsCSSNumber
                : letterSpacingMetadata.value
            }
            onSubmitValue={wrappedLetterSpacingOnSubmitValue}
            onTransientSubmitValue={wrappedLetterSpacingOnTransientSubmitValue}
            controlStatus={letterSpacingMetadata.controlStatus}
            innerLabel={<Icons.LetterSpacing color='on-highlight-secondary' />}
            stepSize={0.01}
            numberType='Length'
            defaultUnitToHide={'px'}
            style={{ flex: 1 }}
          />
          <NumberInput
            key='lineHeight'
            id='lineHeight'
            testId='lineHeight'
            value={
              lineHeightMetadata.value === 'normal'
                ? normalLineHeightAsCSSNumber
                : lineHeightMetadata.value
            }
            controlStatus={lineHeightMetadata.controlStatus}
            onSubmitValue={wrappedLineHeightOnSubmitValue}
            onTransientSubmitValue={wrappedLineHeightOnTransientSubmitValue}
            innerLabel={<Icons.LineHeight color='on-highlight-secondary' />}
            stepSize={0.01}
            numberType='Length'
            defaultUnitToHide={'em'}
            style={{ flex: 1 }}
          />
        </FlexRow>
      ) : null}
    </div>
  )
})
TextSubsection.displayName = 'TextSubsection'
