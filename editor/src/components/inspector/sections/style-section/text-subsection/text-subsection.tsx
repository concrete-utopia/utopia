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
  Icn,
  colorTheme,
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
import { OptionChainControl, OptionChainOption } from '../../../controls/option-chain-control'
import { OptionControl } from '../../../controls/option-control'
import { PropertyRow } from '../../../widgets/property-row'
import { FontFamilySelect } from './font-family-select'
import { FontVariantSelect } from './font-variant-select'
import { FlexRow } from 'utopia-api'
import { getControlStyles } from '../../../common/control-styles'
import { Utils } from '../../../../../uuiui-deps'
import { TextRelatedProperties } from '../../../../../core/properties/css-properties'
import { useContextSelector } from 'use-context-selector'
import { TextAutoSizingControl } from './text-auto-sizing-control'

const ObjectPathImmutable: any = OPI

function updateItalicFontStyle(newValue: boolean, oldValue: CSSFontStyle): CSSFontStyle {
  return newValue ? 'italic' : 'normal'
}

function updateUnderlinedTextDecoration(newValue: boolean): CSSTextDecorationLine {
  return newValue ? 'underline' : 'none'
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

  return (
    <>
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
          <Icons.Threedots color={expanded ? 'secondary' : 'subdued'} onClick={toggleExpanded} />
        </InspectorSubsectionHeader>
      </InspectorContextMenuWrapper>
      <FontFamilySelect />
      <PropertyRow style={{ gridColumnGap: 8, gridTemplateColumns: '130px 55px 28px' }}>
        <FontVariantSelect />
        <InspectorContextMenuWrapper
          id='fontSize-context-menu'
          items={fontSizeContextMenuItems}
          data={null}
          style={{ gridColumn: '2' }}
        >
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
          />
        </InspectorContextMenuWrapper>
        <InspectorContextMenuWrapper
          id='color-context-menu'
          items={colorContextMenuItems}
          data={null}
          style={{ gridColumn: '3' }}
        >
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
        </InspectorContextMenuWrapper>
      </PropertyRow>
      <PropertyRow style={{ gridColumnGap: 8 }}>
        <TextAutoSizingControl />
        <InspectorContextMenuWrapper
          id='textAlign-context-menu'
          items={textAlignContextMenuItems}
          data={null}
          style={{ gridColumn: '4 / span 3' }}
        >
          <OptionChainControl
            id='textAlign'
            key='textAlign'
            testId='textAlign'
            value={textAlignMetadata.value}
            onSubmitValue={textAlignMetadata.onSubmitValue}
            controlStatus={textAlignMetadata.controlStatus}
            controlStyles={textAlignMetadata.controlStyles}
            options={[
              {
                value: 'left',
                icon: {
                  category: 'typography',
                  type: 'leftAlign',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              },
              {
                value: 'center',
                icon: {
                  category: 'typography',
                  type: 'centerAlign',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              },
              {
                value: 'right',
                icon: {
                  category: 'typography',
                  type: 'rightAlign',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              },
              {
                value: 'justify',
                icon: {
                  category: 'typography',
                  type: 'justify',
                  color: 'secondary',
                  width: 16,
                  height: 16,
                },
              },
            ]}
          />
        </InspectorContextMenuWrapper>
      </PropertyRow>
      {expanded ? (
        <PropertyRow style={{ gridColumnGap: 8 }}>
          <InspectorContextMenuWrapper
            id='italic-context-menu'
            items={italicContextMenuItems}
            data={null}
            style={{ gridColumn: '1 / span 1' }}
          >
            <OptionControl
              id='italic'
              key='italic'
              testId='italic'
              value={fontStyleMetadata.value === 'italic'}
              onSubmitValue={onItalicSubmitValue}
              controlStatus={fontStyleMetadata.controlStatus}
              controlStyles={fontStyleMetadata.controlStyles}
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
              style={{ border: `1px solid ${colorTheme.bg2.value}`, borderRadius: 3 }}
            />
          </InspectorContextMenuWrapper>
          <InspectorContextMenuWrapper
            id='textDecorationLine-context-menu'
            items={underlineContextMenuItems}
            data={null}
          >
            <OptionControl
              id='underlined'
              key='underlined'
              testId='underlined'
              value={textDecorationLineMetadata.value === 'underline'}
              onSubmitValue={onUnderlinedSubmitValue}
              controlStatus={textDecorationLineMetadata.controlStatus}
              controlStyles={textDecorationLineMetadata.controlStyles}
              style={{
                gridColumn: '2 / span 1',
                border: `1px solid ${colorTheme.bg2.value}`,
                borderRadius: 3,
              }}
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
          </InspectorContextMenuWrapper>
          <InspectorContextMenuWrapper
            id='letterSpacing-context-menu'
            items={letterSpacingContextMenuItems}
            data={null}
            style={{ gridColumn: '3 / span 2' }}
          >
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
              DEPRECATED_labelBelow='letter'
              stepSize={0.01}
              numberType='Length'
              defaultUnitToHide={'px'}
            />
          </InspectorContextMenuWrapper>
          <InspectorContextMenuWrapper
            id='lineHeight-context-menu'
            items={lineHeightContextMenuItems}
            data={null}
            style={{ gridColumn: '5 / span 2' }}
          >
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
              DEPRECATED_labelBelow='line'
              stepSize={0.01}
              numberType='Length'
              defaultUnitToHide={'em'}
            />
          </InspectorContextMenuWrapper>
        </PropertyRow>
      ) : null}
    </>
  )
})
TextSubsection.displayName = 'TextSubsection'
