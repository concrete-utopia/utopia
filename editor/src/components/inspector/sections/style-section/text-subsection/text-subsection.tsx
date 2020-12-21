import * as OPI from 'object-path-immutable'
import * as React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import {
  isJSXAttributeValue,
  JSXAttribute,
  jsxAttributeValue,
} from '../../../../../core/shared/element-template'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { filterScenes } from '../../../../../core/shared/template-path'
import {
  betterReactMemo,
  useKeepShallowReferenceEquality,
} from '../../../../../utils/react-performance'
import utils from '../../../../../utils/utils'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  InspectorSubsectionHeader,
  Icons,
  NumberInput,
} from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { EditorAction } from '../../../../editor/action-types'
import * as EditorActions from '../../../../editor/actions/action-creators'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { measureTextFieldNew } from '../../../../text-utils'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import { CSSFontStyle, cssNumber, CSSTextDecorationLine } from '../../../common/css-utils'
import { usePropControlledRef_DANGEROUS } from '../../../common/inspector-utils'
import {
  InspectorCallbackContext,
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

const ObjectPathImmutable: any = OPI

function updateItalicFontStyle(newValue: boolean, oldValue: CSSFontStyle): CSSFontStyle {
  return newValue ? 'italic' : 'normal'
}

function updateUnderlinedTextDecoration(newValue: boolean): CSSTextDecorationLine {
  return newValue ? 'underline' : 'none'
}

const normalLetterSpacingAsCSSNumber = cssNumber(0, 'px')
const normalLineHeightAsCSSNumber = cssNumber(0, 'px')

export const TextSubsection = betterReactMemo('TextSubsection', () => {
  const [expanded, setExpanded] = React.useState(false)

  const colorMetadata = useInspectorStyleInfo('color')

  const fontFamilyMetadata = useInspectorStyleInfo('fontFamily')

  const fontStyleMetadata = useInspectorStyleInfo('fontStyle')

  const [onItalicSubmitValue] = fontStyleMetadata.useSubmitValueFactory(updateItalicFontStyle)

  const fontSizeMetadata = useInspectorStyleInfo('fontSize')

  const textAlignMetadata = useInspectorStyleInfo('textAlign')

  const textSizingMetadata = useInspectorElementInfo('textSizing')

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
    textSizingMetadata.controlStyles.unsettable ||
    textDecorationLineMetadata.controlStyles.unsettable ||
    letterSpacingMetadata.controlStyles.unsettable ||
    lineHeightMetadata.controlStyles.unsettable

  const onUnsetSubsectionValues = () => {
    colorMetadata.onUnsetValues()
    fontFamilyMetadata.onUnsetValues()
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
          <div
            style={{
              flexGrow: 1,
            }}
          >
            Type
          </div>
          <Icons.Gear color={expanded ? 'darkgray' : 'lightgray'} onClick={toggleExpanded} />
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
            value={fontSizeMetadata.value}
            controlStatus={fontSizeMetadata.controlStatus}
            onSubmitValue={wrappedFontSizeOnSubmitValue}
            onTransientSubmitValue={wrappedFontSizeOnTransientSubmitValue}
            minimum={0}
            numberType='Length'
            defaultUnitToHide='px'
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
        <InspectorContextMenuWrapper
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
        </InspectorContextMenuWrapper>
        <InspectorContextMenuWrapper
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
              value={fontStyleMetadata.value === 'italic'}
              onSubmitValue={onItalicSubmitValue}
              controlStatus={fontStyleMetadata.controlStatus}
              controlStyles={fontStyleMetadata.controlStyles}
              DEPRECATED_controlOptions={{
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
          </InspectorContextMenuWrapper>
          <InspectorContextMenuWrapper
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
              DEPRECATED_controlOptions={{
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
              defaultUnitToHide='px'
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
            />
          </InspectorContextMenuWrapper>
        </PropertyRow>
      ) : null}
    </>
  )
})
TextSubsection.displayName = 'TextSubsection'

export const AutosizingTextSubsection = betterReactMemo('AutosizingTextSubsection', () => {
  const selectedViewsRef = usePropControlledRef_DANGEROUS(filterScenes(useSelectedViews()))

  const stateRef = useRefEditorState((store) => {
    return {
      dispatch: store.dispatch,
      componentMetadata: store.editor.jsxMetadataKILLME,
    }
  })

  const inspectorContext = React.useContext(InspectorCallbackContext)

  const onSubmitValue = React.useCallback(
    async (newValue: JSXAttribute, propertyPath: PropertyPath, transient: boolean) => {
      const selectedPaths = selectedViewsRef.current

      let actions: Array<EditorAction> = []
      for (let path of selectedPaths) {
        const element = MetadataUtils.getElementByInstancePathMaybe(
          stateRef.current.componentMetadata.elements,
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
    [stateRef, inspectorContext, selectedViewsRef],
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
