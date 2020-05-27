import * as OPI from 'object-path-immutable'
import * as React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import {
  isJSXAttributeValue,
  JSXAttribute,
  jsxAttributeValue,
} from '../../../../../core/shared/element-template'
import Utils from '../../../../../utils/utils'
import { Icons, useWrappedEmptyOnSubmitValue } from 'uuiui'
import { NumberInput } from 'uuiui'
import { Tooltip } from 'uuiui'
import { InspectorSubsectionHeader } from 'uuiui'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import { EditorAction } from '../../../../editor/action-types'
import * as EditorActions from '../../../../editor/actions/actions'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import * as PP from '../../../../../core/shared/property-path'
import { measureTextFieldNew } from '../../../../text-utils'
import { ColorControl } from '../../../controls/color-control'
import { OptionChainControl, OptionChainOption } from '../../../controls/option-chain-control'
import { OptionControl } from '../../../controls/option-control'
import { SelectControl, SelectOption } from '../../../controls/select-control'
import {
  AllTypefacesAlphabeticallySorted,
  cssFontWeightAndStyleToUtopiaFontWeight,
  defaultFontWeightsAndStyles,
  fontFamilyArrayToCSSFontFamilyString,
  fontWeightsList,
  TypefaceInfo,
  UtopiaFontWeight,
} from './fonts-list'
import { PropertyRow } from '../../../widgets/property-row'
import {
  CSSFontFamily,
  CSSFontWeightAndStyle,
  CSSLineHeight,
  CSSTextDecorationLine,
  CSSFontStyle,
  cssNumber,
} from '../../../new-inspector/css-utils'
import {
  InspectorCallbackContext,
  useInspectorStyleInfo,
  useIsSubSectionVisible,
  useKeepShallowReferenceEquality,
  useInspectorMetadataInfo,
  useInspectorInfo,
  stylePropPathMappingFn,
} from '../../../new-inspector/new-inspector-hooks'
import { filterScenes } from '../../../../../core/shared/template-path'
import utils from '../../../../../utils/utils'
import { addOnUnsetValues } from '../../../new-inspector/context-menu-items'
import { NewInspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { betterReactMemo } from 'uuiui-deps'

const ObjectPathImmutable: any = OPI

function getInfoForTypeface(cssFontFamily: CSSFontFamily): TypefaceInfo {
  const value = AllTypefacesAlphabeticallySorted.find(
    (typeface) => typeface.cssFontFamily[0] === cssFontFamily[0],
  )
  if (value != null) {
    return value
  } else {
    return {
      fontFamilyName: cssFontFamily[0],
      cssFontFamily: cssFontFamily,
      fontWeightsAndStyles: defaultFontWeightsAndStyles,
    }
  }
}

function getFontFamilyValue(fontFamily: CSSFontFamily): string {
  const info = getInfoForTypeface(fontFamily)
  const fontFamilyString = fontFamilyArrayToCSSFontFamilyString(info.cssFontFamily)
  return fontFamilyString
}

function convertTypefaceListToSelectOptionList(
  typefaces: Array<TypefaceInfo>,
): Array<SelectOption> {
  return typefaces.map((typeface) => {
    const fontFamily = fontFamilyArrayToCSSFontFamilyString(typeface.cssFontFamily)
    return {
      value: fontFamily,
      label: typeface.fontFamilyName,
      style: {
        fontFamily: fontFamily,
      },
    }
  })
}

function getFontWeightOptions(typeface: TypefaceInfo): Array<SelectOption> {
  return typeface.fontWeightsAndStyles.map((variant) => {
    const utopiaFontWeight = cssFontWeightAndStyleToUtopiaFontWeight(variant)
    const label = fontWeightsList[utopiaFontWeight].fontWeightAndStyleName
    return {
      label: label,
      value: utopiaFontWeight,
      style: {
        ...variant,
        fontFamily: fontFamilyArrayToCSSFontFamilyString(typeface.cssFontFamily),
      },
    }
  })
}

function getFontFamilyOptions(): Array<SelectOption> {
  return convertTypefaceListToSelectOptionList(AllTypefacesAlphabeticallySorted)
}

function getTypefaceInfoFromCSSFontFamily(cssFontFamily: CSSFontFamily): TypefaceInfo {
  const value = AllTypefacesAlphabeticallySorted.find(
    (typeface, i) => typeface.cssFontFamily[0] === cssFontFamily[0],
  )
  if (value) {
    return value
  } else {
    return {
      fontFamilyName: fontFamilyArrayToCSSFontFamilyString(cssFontFamily),
      cssFontFamily,
      fontWeightsAndStyles: defaultFontWeightsAndStyles,
    }
  }
}

function updateItalicFontStyle(newValue: boolean, oldValue: CSSFontStyle): CSSFontStyle {
  return newValue ? 'italic' : 'normal'
}

function utopiaFontWeightToCSSFontWeightAndStyle(
  utopiaFontWeight: UtopiaFontWeight,
): CSSFontWeightAndStyle {
  return {
    fontWeight: parseInt(utopiaFontWeight.slice(0, 3)),
    fontStyle: utopiaFontWeight.endsWith('i') ? 'italic' : 'normal',
  }
}

function updateUnderlinedTextDecoration(newValue: boolean): CSSTextDecorationLine {
  return newValue ? 'underline' : 'none'
}

const weightAndStylePaths: Array<'fontWeight' | 'fontStyle'> = ['fontWeight', 'fontStyle']
const normalLetterSpacingAsCSSNumber = cssNumber(0, 'px')
const normalLineHeightAsCSSNumber = cssNumber(0, 'px')

export const TextSubsection = betterReactMemo('TextSubsection', () => {
  const [expanded, setExpanded] = React.useState(false)

  const colorMetadata = useInspectorStyleInfo('color')

  const fontFamilyMetadata = useInspectorStyleInfo('fontFamily')
  const [onFontFamilySubmitValue] = fontFamilyMetadata.useSubmitValueFactory(
    (newFontfamily: string) => {
      return newFontfamily.split(', ')
    },
  )

  const fontWeightAndStyleMetadata = useInspectorInfo(
    weightAndStylePaths,
    cssFontWeightAndStyleToUtopiaFontWeight,
    utopiaFontWeightToCSSFontWeightAndStyle,
    stylePropPathMappingFn,
  )

  const fontStyleMetadata = useInspectorStyleInfo('fontStyle')

  const [onItalicSubmitValue] = fontStyleMetadata.useSubmitValueFactory(updateItalicFontStyle)

  const fontWeightAndStyleOptions = React.useMemo(
    () => getFontWeightOptions(getTypefaceInfoFromCSSFontFamily(fontFamilyMetadata.value)),
    [fontFamilyMetadata.value],
  )

  const fontSizeMetadata = useInspectorStyleInfo('fontSize')

  const textAlignMetadata = useInspectorStyleInfo('textAlign')

  const textSizingMetadata = useInspectorMetadataInfo('textSizing')

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
    fontWeightAndStyleMetadata.controlStyles.unsettable ||
    fontStyleMetadata.controlStyles.unsettable ||
    fontSizeMetadata.controlStyles.unsettable ||
    textAlignMetadata.controlStyles.unsettable ||
    textSizingMetadata.controlStyles.unsettable ||
    textDecorationLineMetadata.controlStyles.unsettable ||
    letterSpacingMetadata.controlStyles.unsettable ||
    lineHeightMetadata.controlStyles.unsettable

  const onUnsetSubsectionValues = () => {
    colorMetadata.onUnsetValues()
    fontFamilyMetadata.onUnsetValues()
    fontWeightAndStyleMetadata.onUnsetValues()
    fontStyleMetadata.onUnsetValues()
    fontSizeMetadata.onUnsetValues()
    textAlignMetadata.onUnsetValues()
    textSizingMetadata.onUnsetValues()
    textDecorationLineMetadata.onUnsetValues()
    letterSpacingMetadata.onUnsetValues()
    lineHeightMetadata.onUnsetValues()
  }

  const subsectionContextMenuItems = utils.stripNulls([
    showSubsectionUnsetContextMenuItem
      ? addOnUnsetValues(['all font properties'], onUnsetSubsectionValues)
      : null,
  ])

  const fontFamilyContextMenuItems = utils.stripNulls([
    fontFamilyMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['fontFamily'], fontFamilyMetadata.onUnsetValues)
      : null,
  ])

  const fontWeightAndStyleContextMenuItems = utils.stripNulls([
    fontWeightAndStyleMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['fontWeight', 'fontStyle'], fontWeightAndStyleMetadata.onUnsetValues)
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

  const textSizingContextMenuItems = utils.stripNulls([
    textSizingMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['textSizing'], textSizingMetadata.onUnsetValues)
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

  const toggleExpanded = React.useCallback(() => setExpanded((prevExpanded) => !prevExpanded), [
    setExpanded,
  ])

  const wrappedFontSizeOnSubmitValue = useWrappedEmptyOnSubmitValue(
    fontSizeMetadata.onSubmitValue,
    fontSizeMetadata.onUnsetValues,
  )
  const wrappedFontSizeOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    fontSizeMetadata.onTransientSubmitValue,
    fontSizeMetadata.onUnsetValues,
  )

  const wrappedLetterSpacingOnSubmitValue = useWrappedEmptyOnSubmitValue(
    letterSpacingMetadata.onSubmitValue,
    letterSpacingMetadata.onUnsetValues,
  )
  const wrappedLetterSpacingOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    letterSpacingMetadata.onTransientSubmitValue,
    letterSpacingMetadata.onUnsetValues,
  )

  const wrappedLineHeightOnSubmitValue = useWrappedEmptyOnSubmitValue(
    lineHeightMetadata.onSubmitValue,
    lineHeightMetadata.onUnsetValues,
  )
  const wrappedLineHeightOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    lineHeightMetadata.onTransientSubmitValue,
    lineHeightMetadata.onUnsetValues,
  )

  if (!isVisible) {
    return null
  }

  return (
    <>
      <NewInspectorContextMenuWrapper
        id='text-subsection-context-menu'
        items={subsectionContextMenuItems}
        data={null}
      >
        <InspectorSubsectionHeader>
          <div
            style={{
              flexGrow: 1,
            }}
          >
            Type
          </div>
          <Icons.Gear color={expanded ? 'darkgray' : 'lightgray'} onClick={toggleExpanded} />
        </InspectorSubsectionHeader>
      </NewInspectorContextMenuWrapper>
      <PropertyRow style={{ gridColumnGap: 8, marginBottom: 8 }}>
        <NewInspectorContextMenuWrapper
          id='fontFamily-context-menu'
          items={fontFamilyContextMenuItems}
          data={null}
          style={{ gridColumn: '1 / span 6' }}
        >
          <SelectControl
            id='font-family'
            key='font-family'
            style={{ fontSize: 14, height: 30 }}
            value={getFontFamilyValue(fontFamilyMetadata.value)}
            controlStatus={fontFamilyMetadata.controlStatus}
            controlStyles={fontFamilyMetadata.controlStyles}
            onSubmitValue={onFontFamilySubmitValue}
            controlOptions={{
              tooltip: 'Typeface',
              creatable: true,
            }}
            options={getFontFamilyOptions()}
          />
        </NewInspectorContextMenuWrapper>
      </PropertyRow>
      <PropertyRow style={{ gridColumnGap: 8, gridTemplateColumns: '130px 55px 28px' }}>
        <NewInspectorContextMenuWrapper
          id='fontWeightAndStyle-context-menu'
          items={fontWeightAndStyleContextMenuItems}
          data={null}
          style={{ gridColumn: '1' }}
        >
          <Tooltip title='Font Weight and Style' placement='top'>
            <SelectControl
              id={'fontWeightAndStyle'}
              key={'fontWeightAndStyle'}
              onSubmitValue={fontWeightAndStyleMetadata.onSubmitValue}
              value={fontWeightAndStyleMetadata.value}
              options={fontWeightAndStyleOptions}
              controlStatus={fontWeightAndStyleMetadata.controlStatus}
              controlStyles={fontWeightAndStyleMetadata.controlStyles}
            />
          </Tooltip>
        </NewInspectorContextMenuWrapper>
        <NewInspectorContextMenuWrapper
          id='fontSize-context-menu'
          items={fontSizeContextMenuItems}
          data={null}
          style={{ gridColumn: '2' }}
        >
          <NumberInput
            id='fontSize'
            value={fontSizeMetadata.value}
            controlStatus={fontSizeMetadata.controlStatus}
            onSubmitValue={wrappedFontSizeOnSubmitValue}
            onTransientSubmitValue={wrappedFontSizeOnTransientSubmitValue}
            minimum={0}
            numberType='Length'
            defaultUnitToHide='px'
          />
        </NewInspectorContextMenuWrapper>
        <NewInspectorContextMenuWrapper
          id='color-context-menu'
          items={colorContextMenuItems}
          data={null}
          style={{ gridColumn: '3' }}
        >
          <ColorControl
            id='color-control'
            key='color-control'
            value={colorMetadata.value}
            onSubmitValue={colorMetadata.onSubmitValue}
            onTransientSubmitValue={colorMetadata.onTransientSubmitValue}
            pickerOffset={{ x: -223, y: 0 }}
            controlStatus={colorMetadata.controlStatus}
            controlStyles={colorMetadata.controlStyles}
          />
        </NewInspectorContextMenuWrapper>
      </PropertyRow>
      <PropertyRow style={{ gridColumnGap: 8 }}>
        <NewInspectorContextMenuWrapper
          id='textSizing-context-menu'
          items={textSizingContextMenuItems}
          data={null}
          style={{ gridColumn: '1 / span 2' }}
        >
          <OptionChainControl
            id='textAutoSizing'
            key='textAutoSizing'
            controlStatus={textSizingMetadata.controlStatus}
            controlStyles={textSizingMetadata.controlStyles}
            onSubmitValue={textSizingMetadata.onSubmitValue}
            value={textSizingMetadata.value}
            options={
              [
                {
                  value: 'auto',
                  tooltip: 'Auto',
                  icon: {
                    category: 'typography',
                    type: 'auto',
                    color: 'darkgray',
                    width: 16,
                    height: 16,
                  },
                },
                {
                  value: 'fixed',
                  tooltip: 'Fixed',
                  icon: {
                    category: 'typography',
                    type: 'fixed',
                    color: 'darkgray',
                    width: 16,
                    height: 16,
                  },
                },
              ] as Array<OptionChainOption<string | number>>
            }
          />
        </NewInspectorContextMenuWrapper>
        <NewInspectorContextMenuWrapper
          id='textAlign-context-menu'
          items={textAlignContextMenuItems}
          data={null}
          style={{ gridColumn: '3 / span 4' }}
        >
          <OptionChainControl
            id='textAlign'
            key='textAlign'
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
                  color: 'darkgray',
                  width: 16,
                  height: 16,
                },
              },
              {
                value: 'center',
                icon: {
                  category: 'typography',
                  type: 'centerAlign',
                  color: 'darkgray',
                  width: 16,
                  height: 16,
                },
              },
              {
                value: 'right',
                icon: {
                  category: 'typography',
                  type: 'rightAlign',
                  color: 'darkgray',
                  width: 16,
                  height: 16,
                },
              },
              {
                value: 'justify',
                icon: {
                  category: 'typography',
                  type: 'justify',
                  color: 'darkgray',
                  width: 16,
                  height: 16,
                },
              },
            ]}
          />
        </NewInspectorContextMenuWrapper>
      </PropertyRow>
      {expanded ? (
        <PropertyRow style={{ gridColumnGap: 8 }}>
          <NewInspectorContextMenuWrapper
            id='italic-context-menu'
            items={italicContextMenuItems}
            data={null}
            style={{ gridColumn: '1 / span 1' }}
          >
            <OptionControl
              id='italic'
              key='italic'
              value={fontStyleMetadata.value === 'italic'}
              onSubmitValue={onItalicSubmitValue}
              controlStatus={fontStyleMetadata.controlStatus}
              controlStyles={fontStyleMetadata.controlStyles}
              controlOptions={{
                tooltip: 'Italic',
                icon: {
                  category: 'typography',
                  type: 'italic',
                  color: 'darkgray',
                  width: 16,
                  height: 16,
                },
              }}
            />
          </NewInspectorContextMenuWrapper>
          <NewInspectorContextMenuWrapper
            id='textDecorationLine-context-menu'
            items={underlineContextMenuItems}
            data={null}
          >
            <OptionControl
              id='underlined'
              key='underlined'
              value={textDecorationLineMetadata.value === 'underline'}
              onSubmitValue={onUnderlinedSubmitValue}
              controlStatus={textDecorationLineMetadata.controlStatus}
              controlStyles={textDecorationLineMetadata.controlStyles}
              style={{ gridColumn: '2 / span 1' }}
              controlOptions={{
                tooltip: 'Underline',
                icon: {
                  category: 'typography',
                  type: 'underline',
                  color: 'darkgray',
                  width: 16,
                  height: 16,
                },
              }}
            />
          </NewInspectorContextMenuWrapper>
          <NewInspectorContextMenuWrapper
            id='letterSpacing-context-menu'
            items={letterSpacingContextMenuItems}
            data={null}
            style={{ gridColumn: '3 / span 2' }}
          >
            <NumberInput
              key='letterSpacing'
              id='letterSpacing'
              value={
                letterSpacingMetadata.value === 'normal'
                  ? normalLetterSpacingAsCSSNumber
                  : letterSpacingMetadata.value
              }
              onSubmitValue={wrappedLetterSpacingOnSubmitValue}
              onTransientSubmitValue={wrappedLetterSpacingOnTransientSubmitValue}
              controlStatus={letterSpacingMetadata.controlStatus}
              labelBelow='letter'
              stepSize={0.01}
              numberType='Length'
              defaultUnitToHide='px'
            />
          </NewInspectorContextMenuWrapper>
          <NewInspectorContextMenuWrapper
            id='lineHeight-context-menu'
            items={lineHeightContextMenuItems}
            data={null}
            style={{ gridColumn: '5 / span 2' }}
          >
            <NumberInput
              key='lineHeight'
              id='lineHeight'
              value={
                lineHeightMetadata.value === 'normal'
                  ? normalLineHeightAsCSSNumber
                  : lineHeightMetadata.value
              }
              controlStatus={lineHeightMetadata.controlStatus}
              onSubmitValue={wrappedLineHeightOnSubmitValue}
              onTransientSubmitValue={wrappedLineHeightOnTransientSubmitValue}
              labelBelow='line'
              stepSize={0.01}
              numberType='Length'
            />
          </NewInspectorContextMenuWrapper>
        </PropertyRow>
      ) : null}
    </>
  )
})
TextSubsection.displayName = 'TextSubsection'

export const AutosizingTextSubsection = betterReactMemo('AutosizingTextSubsection', () => {
  const stateRef = useRefEditorState((store) => {
    return {
      dispatch: store.dispatch,
      selectedViews: filterScenes(store.editor.selectedViews),
      componentMetadata: store.editor.jsxMetadataKILLME,
    }
  })

  const inspectorContext = React.useContext(InspectorCallbackContext)

  const onSubmitValue = React.useCallback(
    async (newValue: JSXAttribute, propertyPath: PropertyPath, transient: boolean) => {
      const selectedPaths = stateRef.current.selectedViews

      let actions: Array<EditorAction> = []
      for (let path of selectedPaths) {
        const element = MetadataUtils.getElementByInstancePathMaybe(
          stateRef.current.componentMetadata,
          path,
        )

        // if this is a value attribute update, let's resize autosizing text, because property change can
        // affect size
        if (isJSXAttributeValue(newValue)) {
          const rawNewValue = newValue.value

          const settingTextSizingToAuto =
            PP.pathsEqual(propertyPath, PP.create(['textSizing'])) && rawNewValue == 'auto'

          if (element != null && (element.props.textSizing == 'auto' || settingTextSizingToAuto)) {
            // apply the current change to the prop for proper size measurement
            const updatedProps = ObjectPathImmutable.set(
              element.props,
              propertyPath.propertyElements as any,
              rawNewValue,
            )
            const updatedElement = {
              ...element,
              props: updatedProps,
            }
            const size = await measureTextFieldNew(updatedElement)
            if (size.width != null && size.height != null) {
              actions.push(EditorActions.updateFrameDimensions(path, size.width, size.height))
            }
          }
          // if this is not a value attribute, set the text to fixed sizing, because we can not measure it anyway
        } else if (element != null && element.props.textSizing == 'auto') {
          actions.push(
            EditorActions.setProp_UNSAFE(
              path,
              PP.create(['textSizing']),
              jsxAttributeValue('fixed'),
            ),
          )
        }
      }
      stateRef.current.dispatch(actions, 'inspector')
      inspectorContext.onSubmitValue(newValue, propertyPath, transient)
    },
    [stateRef, inspectorContext],
  )

  const updatedContext = useKeepShallowReferenceEquality({
    ...inspectorContext,
    onSubmitValue: onSubmitValue,
  })

  return (
    <>
      <InspectorCallbackContext.Provider value={updatedContext}>
        <TextSubsection />
      </InspectorCallbackContext.Provider>
    </>
  )
})
AutosizingTextSubsection.displayName = 'AutosizingTextSubsection'
