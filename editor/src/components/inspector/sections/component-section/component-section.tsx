import * as React from 'react'
import { OptionsType } from 'react-select'
import { animated } from 'react-spring'
import {
  ArrayControlDescription,
  BaseControlDescription,
  ControlDescription,
  isBaseControlDescription,
  ObjectControlDescription,
  UnionControlDescription,
} from 'utopia-api'
import { PathForSceneProps } from '../../../../core/model/scene-utils'
import {
  filterSpecialProps,
  getDescriptionUnsetOptionalFields,
} from '../../../../core/property-controls/property-controls-utils'
import { joinSpecial } from '../../../../core/shared/array-utils'
import { eitherToMaybe, foldEither, maybeEitherToMaybe } from '../../../../core/shared/either'
import { mapToArray } from '../../../../core/shared/object-utils'
import { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../../utils/react-performance'
import Utils from '../../../../utils/utils'
import { getParseErrorDetails, ParseError } from '../../../../utils/value-parser-utils'
import {
  Tooltip,
  colorTheme,
  UtopiaTheme,
  InspectorSectionHeader,
  SimpleFlexRow,
  SquareButton,
  Icn,
  PopupList,
  FunctionIcons,
  Icons,
  LargerIcons,
  Section,
  SectionBodyArea,
  FlexColumn,
  paddingTop,
  Subdued,
  VerySubdued,
} from '../../../../uuiui'
import { getControlStyles } from '../../../../uuiui-deps'
import { InfoBox } from '../../../common/notices'
import { InspectorContextMenuWrapper } from '../../../context-menu-wrapper'
import {
  openCodeEditorFile,
  setFocusedElement,
  showContextMenu,
} from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import { addOnUnsetValues } from '../../common/context-menu-items'
import { InstanceContextMenu } from '../../common/instance-context-menu'
import {
  useControlForUnionControl,
  useInspectorInfoForPropertyControl,
} from '../../common/property-controls-hooks'
import {
  useGivenPropsWithoutControls,
  useSelectedPropertyControls,
  useUsedPropsWithoutControls,
  useUsedPropsWithoutDefaults,
} from '../../common/property-path-hooks'
import { useArraySuperControl } from '../../controls/array-supercontrol'
import { SelectOption } from '../../controls/select-control'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { PropertyRow } from '../../widgets/property-row'
import {
  ControlForBooleanProp,
  ControlForColorProp,
  ControlForComponentInstanceProp,
  ControlForEnumProp,
  ControlForEventHandlerProp,
  ControlForImageProp,
  ControlForNumberProp,
  ControlForOptionsProp,
  ControlForPopupListProp,
  ControlForPropProps,
  ControlForSliderProp,
  ControlForStringProp,
} from './property-control-controls'
import { IconToggleButton } from '../../../../uuiui/icon-toggle-button'
import { InlineButton, InlineLink } from '../../../../uuiui/inline-button'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getJSXElementNameAsString, isJSXElement } from '../../../../core/shared/element-template'
import { normalisePathToUnderlyingTarget } from '../../../custom-code/code-file'
import { usePackageDependencies } from '../../../editor/npm-dependency/npm-dependency'
import {
  getFilePathForImportedComponent,
  isAnimatedElement,
  isImportedComponentNPM,
} from '../../../../core/model/project-file-utils'

function useComponentPropsInspectorInfo(
  partialPath: PropertyPath,
  addPropsToPath: boolean,
  control: ControlDescription,
) {
  const propertyPath = addPropsToPath ? PP.append(PathForSceneProps, partialPath) : partialPath
  return useInspectorInfoForPropertyControl(propertyPath, control)
}

const ControlForProp = betterReactMemo(
  'ControlForProp',
  (props: ControlForPropProps<BaseControlDescription>) => {
    const { controlDescription } = props
    if (controlDescription == null) {
      return null
    } else {
      switch (controlDescription.type) {
        case 'boolean':
          return <ControlForBooleanProp {...props} controlDescription={controlDescription} />
        case 'color':
          return <ControlForColorProp {...props} controlDescription={controlDescription} />
        case 'componentinstance':
          return (
            <ControlForComponentInstanceProp {...props} controlDescription={controlDescription} />
          )
        case 'enum':
          return <ControlForEnumProp {...props} controlDescription={controlDescription} />
        case 'eventhandler':
          return <ControlForEventHandlerProp {...props} controlDescription={controlDescription} />
        case 'ignore':
          return null
        case 'image':
          return <ControlForImageProp {...props} controlDescription={controlDescription} />
        case 'number':
          return <ControlForNumberProp {...props} controlDescription={controlDescription} />
        case 'options':
          return <ControlForOptionsProp {...props} controlDescription={controlDescription} />
        case 'popuplist':
          return <ControlForPopupListProp {...props} controlDescription={controlDescription} />
        case 'slider':
          return <ControlForSliderProp {...props} controlDescription={controlDescription} />
        case 'string':
          return <ControlForStringProp {...props} controlDescription={controlDescription} />
        // case 'styleobject':
        default:
          return null
      }
    }
  },
)

interface ParseErrorProps {
  parseError: ParseError
}

export const ParseErrorControl = betterReactMemo('ParseErrorControl', (props: ParseErrorProps) => {
  const details = getParseErrorDetails(props.parseError)
  return (
    <div>
      <Tooltip title={`${details.path}`}>
        <span>{details.description}</span>
      </Tooltip>
    </div>
  )
})

const WarningTooltip = betterReactMemo('WarningTooltip', ({ warning }: { warning: string }) => {
  return (
    <Tooltip title={warning}>
      <div
        style={{
          width: 5,
          height: 5,
          background: colorTheme.warningBgSolid.value,
          borderRadius: '50%',
          marginRight: 4,
        }}
      />
    </Tooltip>
  )
})

interface RowForInvalidControlProps {
  propName: string
  title: string
  propertyError: ParseError
  warningTooltip?: string
}

const RowForInvalidControl = betterReactMemo(
  'RowForInvalidControl',
  (props: RowForInvalidControlProps) => {
    const propPath = [PP.create([props.propName])]
    const warning =
      props.warningTooltip == null ? null : <WarningTooltip warning={props.warningTooltip} />
    return (
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={propPath}>
          {warning}
          {props.title}
        </PropertyLabel>
        <ParseErrorControl parseError={props.propertyError} />
      </UIGridRow>
    )
  },
)

interface AbstractRowForControlProps {
  propPath: PropertyPath
  isScene: boolean
}

function titleForControl(propPath: PropertyPath, control: ControlDescription): string {
  return control.title ?? PP.lastPartToString(propPath)
}

interface RowForBaseControlProps extends AbstractRowForControlProps {
  label?: React.ComponentType<any>
  controlDescription: BaseControlDescription
}

const RowForBaseControl = betterReactMemo('RowForBaseControl', (props: RowForBaseControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = titleForControl(propPath, controlDescription)
  const propName = `${PP.lastPart(propPath)}`

  let warningTooltip: string | undefined = undefined
  const unsetOptionalFields = getDescriptionUnsetOptionalFields(controlDescription)
  if (unsetOptionalFields.length > 0) {
    warningTooltip = `These optional fields are not set: ${joinSpecial(
      unsetOptionalFields,
      ', ',
      ' and ',
    )}`
  }

  const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([propName], propMetadata.onUnsetValues),
  ])
  const warning = warningTooltip == null ? null : <WarningTooltip warning={warningTooltip} />

  const propertyLabel =
    props.label == null ? (
      <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize' }}>
        {warning}
        {title}
      </PropertyLabel>
    ) : (
      <props.label />
    )

  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${propName}`}
      items={contextMenuItems}
      data={null}
    >
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        {propertyLabel}
        <ControlForProp
          propName={propName}
          controlDescription={controlDescription}
          propMetadata={propMetadata}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

interface RowForArrayControlProps extends AbstractRowForControlProps {
  controlDescription: ArrayControlDescription
}

const RowForArrayControl = betterReactMemo(
  'RowForArrayControl',
  (props: RowForArrayControlProps) => {
    const { propPath, controlDescription, isScene } = props
    const title = titleForControl(propPath, controlDescription)
    const { value, onSubmitValue, propertyStatus } = useComponentPropsInspectorInfo(
      propPath,
      isScene,
      controlDescription,
    )

    const rowHeight = UtopiaTheme.layout.rowHeight.max
    const { springs, bind } = useArraySuperControl(value, onSubmitValue, rowHeight, false)
    const [insertingRow, setInsertingRow] = React.useState(false)

    let warningTooltip: string | undefined = undefined
    const unsetOptionalFields = getDescriptionUnsetOptionalFields(controlDescription)
    if (unsetOptionalFields.length > 0) {
      warningTooltip = `These optional fields are not set: ${joinSpecial(
        unsetOptionalFields,
        ', ',
        ' and ',
      )}`
    }
    const warning = warningTooltip == null ? null : <WarningTooltip warning={warningTooltip} />

    const toggleInsertRow = React.useCallback(() => setInsertingRow((current) => !current), [])

    React.useEffect(() => setInsertingRow(false), [springs.length])

    return (
      <>
        <InspectorSectionHeader>
          <SimpleFlexRow style={{ flexGrow: 1 }}>
            <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize' }}>
              {warning}
              {title}
            </PropertyLabel>
            {propertyStatus.overwritable ? (
              <SquareButton highlight onMouseDown={toggleInsertRow}>
                {insertingRow ? (
                  <Icn
                    style={{ paddingTop: 1 }}
                    category='semantic'
                    type='minus'
                    color={propertyStatus.controlled ? 'blue' : 'darkgray'}
                    width={16}
                    height={16}
                  />
                ) : (
                  <Icn
                    style={{ paddingTop: 1 }}
                    category='semantic'
                    type='plus'
                    color={propertyStatus.controlled ? 'blue' : 'darkgray'}
                    width={16}
                    height={16}
                  />
                )}
              </SquareButton>
            ) : null}
          </SimpleFlexRow>
        </InspectorSectionHeader>
        <div
          style={{
            height: rowHeight * springs.length,
          }}
        >
          {springs.map((springStyle, index) => {
            return (
              <animated.div
                {...bind(index)}
                key={index}
                style={{
                  ...springStyle,
                  width: '100%',
                  position: 'absolute',
                  height: rowHeight,
                }}
              >
                <RowForControl
                  controlDescription={controlDescription.propertyControl}
                  isScene={isScene}
                  propPath={PP.appendPropertyPathElems(propPath, [index])}
                />
              </animated.div>
            )
          })}
        </div>
        {insertingRow ? (
          <RowForControl
            controlDescription={controlDescription.propertyControl}
            isScene={isScene}
            propPath={PP.appendPropertyPathElems(propPath, [springs.length])}
          />
        ) : null}
      </>
    )
  },
)

interface RowForObjectControlProps extends AbstractRowForControlProps {
  controlDescription: ObjectControlDescription
}

const RowForObjectControl = betterReactMemo(
  'RowForObjectControl',
  (props: RowForObjectControlProps) => {
    const { propPath, controlDescription, isScene } = props
    const title = titleForControl(propPath, controlDescription)

    let warningTooltip: string | undefined = undefined
    const unsetOptionalFields = getDescriptionUnsetOptionalFields(controlDescription)
    if (unsetOptionalFields.length > 0) {
      warningTooltip = `These optional fields are not set: ${joinSpecial(
        unsetOptionalFields,
        ', ',
        ' and ',
      )}`
    }
    const warning = warningTooltip == null ? null : <WarningTooltip warning={warningTooltip} />

    return (
      <>
        <InspectorSectionHeader>
          <SimpleFlexRow style={{ flexGrow: 1 }}>
            <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize' }}>
              {warning}
              {title}
            </PropertyLabel>
          </SimpleFlexRow>
        </InspectorSectionHeader>
        {mapToArray((innerControl: ControlDescription, prop: string) => {
          const innerPropPath = PP.appendPropertyPathElems(propPath, [prop])
          return (
            <>
              {innerControl.type}
              <RowForControl
                key={`object-control-row-${PP.toString(innerPropPath)}`}
                controlDescription={innerControl}
                isScene={isScene}
                propPath={innerPropPath}
              />
            </>
          )
        }, controlDescription.object)}
      </>
    )
  },
)

interface RowForUnionControlProps extends AbstractRowForControlProps {
  controlDescription: UnionControlDescription
}

const RowForUnionControl = betterReactMemo(
  'RowForUnionControl',
  (props: RowForUnionControlProps) => {
    const { propPath, controlDescription } = props
    const title = titleForControl(propPath, controlDescription)

    const suitableControl = useControlForUnionControl(propPath, controlDescription)
    const [controlToUse, setControlToUse] = React.useState(suitableControl)

    const labelOptions: OptionsType<SelectOption> = controlDescription.controls.map((control) => {
      const label = control.title ?? control.type
      return {
        value: control,
        label: label,
      }
    })

    const onLabelChangeValue = React.useCallback(
      (option: SelectOption) => {
        if (option.value !== controlToUse) {
          setControlToUse(option.value)
        }
      },
      [controlToUse, setControlToUse],
    )

    const simpleControlStyles = getControlStyles('simple')

    const label = React.useMemo(
      () => (
        <PopupList
          value={{
            value: controlToUse,
            label: title,
          }}
          options={labelOptions}
          onSubmitValue={onLabelChangeValue}
          containerMode='showBorderOnHover'
          controlStyles={simpleControlStyles}
          style={{
            maxWidth: '100%',
            overflow: 'hidden',
          }}
        />
      ),
      [controlToUse, labelOptions, onLabelChangeValue, simpleControlStyles, title],
    )

    const labelAsRenderProp = React.useCallback(() => label, [label])

    if (isBaseControlDescription(controlToUse)) {
      return (
        <RowForBaseControl {...props} label={labelAsRenderProp} controlDescription={controlToUse} />
      )
    } else {
      return (
        <>
          {label}
          <RowForControl {...props} controlDescription={controlToUse} />
        </>
      )
    }
  },
)

interface RowForControlProps extends AbstractRowForControlProps {
  controlDescription: ControlDescription
}

const RowForControl = betterReactMemo('RowForControl', (props: RowForControlProps) => {
  const { controlDescription } = props

  if (isBaseControlDescription(controlDescription)) {
    return <RowForBaseControl {...props} controlDescription={controlDescription} />
  } else {
    switch (controlDescription.type) {
      case 'array':
        return <RowForArrayControl {...props} controlDescription={controlDescription} />
      case 'object':
        return <RowForObjectControl {...props} controlDescription={controlDescription} />
      case 'union':
        return <RowForUnionControl {...props} controlDescription={controlDescription} />
      default:
        const _exhaustiveCheck: never = controlDescription
        throw new Error(`Unhandled control ${JSON.stringify(controlDescription)}`)
    }
  }
})

function useComponentType(path: ElementPath): string | null {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadata
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
    if (MetadataUtils.isProbablySceneFromMetadata(metadata, path)) {
      return 'Scene'
    }
    if (MetadataUtils.isEmotionOrStyledComponent(path, metadata)) {
      return 'Styled Component'
    }
    const isAnimatedComponent = isAnimatedElement(elementMetadata)
    if (isAnimatedComponent) {
      return 'Animated Component'
    }
    const isImported = isImportedComponentNPM(elementMetadata)
    if (isImported) {
      return 'Component'
    }
    const isComponent = MetadataUtils.isFocusableComponent(path, metadata)
    return isComponent ? 'Component' : null
  }, 'useComponentType')
}

export interface ComponentSectionProps {
  isScene: boolean
}

export const ComponentSectionInner = betterReactMemo(
  'ComponentSectionInner',
  (props: ComponentSectionProps) => {
    const propertyControls = useKeepReferenceEqualityIfPossible(useSelectedPropertyControls(false))
    const propsGivenToElement = useKeepReferenceEqualityIfPossible(useGivenPropsWithoutControls())
    const propsUsedWithoutControls = useKeepReferenceEqualityIfPossible(
      useUsedPropsWithoutControls(propsGivenToElement),
    )
    const dispatch = useEditorState((state) => state.dispatch, 'ComponentSectionInner')

    const selectedViews = useEditorState(
      (store) => store.editor.selectedViews,
      'ComponentSectionInner selectedViews',
    )

    const focusedElementPath = useEditorState(
      (store) => store.editor.focusedElementPath,
      'ComponentSectionInner focusedElementPath',
    )

    const target = selectedViews[0]

    const isFocused = EP.isFocused(focusedElementPath, target)

    const toggleFocusMode = React.useCallback(() => {
      dispatch([setFocusedElement(isFocused ? null : target)])
    }, [dispatch, isFocused, target])

    const locationOfComponentInstance = useEditorState((state) => {
      const element = MetadataUtils.findElementByElementPath(state.editor.jsxMetadata, target)
      const importResult = getFilePathForImportedComponent(element)
      if (importResult == null) {
        const underlyingTarget = normalisePathToUnderlyingTarget(
          state.editor.projectContents,
          state.editor.nodeModules.files,
          state.editor.canvas.openFile?.filename ?? '',
          selectedViews[0],
        )

        return underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' ? underlyingTarget.filePath : null
      } else {
        return importResult
      }
    }, 'ComponentSectionInner locationOfComponentInstance')

    const componentPackageName = useEditorState((state) => {
      const componentMetadata = MetadataUtils.findElementByElementPath(
        state.editor.jsxMetadata,
        target,
      )
      return maybeEitherToMaybe(componentMetadata?.importInfo)?.path
    }, 'ComponentSectionInner componentPackageName')

    const componentPackageMgrLink = `https://www.npmjs.com/package/${componentPackageName}`

    const isFocusable = useEditorState((state) => {
      return MetadataUtils.isFocusableComponent(target, state.editor.jsxMetadata)
    }, 'ComponentSectionInner isFocusable')
    const isImportedComponent = useEditorState((state) => {
      const componentMetadata = MetadataUtils.findElementByElementPath(
        state.editor.jsxMetadata,
        target,
      )
      return isImportedComponentNPM(componentMetadata)
    }, 'ComponentSectionInner isImportedComponent')

    const componentType = useComponentType(target)

    const OpenFile = React.useCallback(() => {
      if (locationOfComponentInstance != null) {
        dispatch([openCodeEditorFile(locationOfComponentInstance, true)])
      }
    }, [dispatch, locationOfComponentInstance])

    return (
      <>
        <InspectorSectionHeader>
          <UIGridRow
            padded={false}
            variant='|--32px--|<--------auto-------->'
            style={{ flexGrow: 1, color: colorTheme.primary.value }}
          >
            Component
          </UIGridRow>
        </InspectorSectionHeader>

        {/* Information about the component as a whole */}
        {isImportedComponent ? (
          <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
            <span
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              <LargerIcons.NpmLogo />
            </span>
            <p>
              {`This ${componentType} is imported from `}
              <InlineLink href={componentPackageMgrLink}>
                {`${componentPackageName}`}
              </InlineLink>{' '}
              via NPM.
            </p>
          </UIGridRow>
        ) : isFocusable && !isFocused ? (
          <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
            <span
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              <IconToggleButton
                value={false}
                srcOn={`/editor/icons/light/element/componentinstance-purple-18x18@2x.png`}
                srcOff={`/editor/icons/light/element/componentinstance-black-18x18@2x.png`}
                onToggle={toggleFocusMode}
              />
            </span>
            <p>
              {`This ${componentType} is imported from `}
              <InlineLink onClick={OpenFile}>{locationOfComponentInstance}</InlineLink>{' '}
              <InlineButton onClick={toggleFocusMode}>Edit it</InlineButton>
            </p>
          </UIGridRow>
        ) : isFocusable && isFocused ? (
          <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
            <span
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              <IconToggleButton
                value={true}
                srcOn={`/editor/icons/light/element/component-purple-18x18@2x.png`}
                srcOff={`/editor/icons/light/element/component-black-18x18@2x.png`}
                onToggle={toggleFocusMode}
              />
            </span>
            <p>
              {`This ${componentType} is imported from `}
              <InlineLink onClick={OpenFile}>{locationOfComponentInstance}</InlineLink>
              <InlineButton onClick={toggleFocusMode}>Exit Editing</InlineButton>
            </p>
          </UIGridRow>
        ) : null}

        {/* List of component props with controls */}
        {foldEither(
          (rootParseError) => {
            return <ParseErrorControl parseError={rootParseError} />
          },
          (rootParseSuccess) => {
            const propNames = filterSpecialProps(Object.keys(rootParseSuccess))
            if (propNames.length > 0) {
              return (
                <>
                  {propNames.map((propName) => {
                    const propertyControl = rootParseSuccess[propName]
                    if (propertyControl == null) {
                      return `${propName} has no property control`
                    } else {
                      return foldEither(
                        (propertyError) => {
                          return (
                            <RowForInvalidControl
                              key={propName}
                              title={propName}
                              propName={propName}
                              propertyError={propertyError}
                            />
                          )
                        },
                        (controlDescription) => {
                          return (
                            <UIGridRow
                              padded
                              tall={false}
                              variant='<-------------1fr------------->'
                            >
                              <RowForControl
                                key={propName}
                                propPath={PP.create([propName])}
                                controlDescription={controlDescription}
                                isScene={props.isScene}
                              />
                            </UIGridRow>
                          )
                        },
                        propertyControl,
                      )
                    }
                  })}
                </>
              )
            } else {
              return null
            }
          },
          propertyControls,
        )}
        {/** props set on the component instance and props used inside the component code */}
        {propsUsedWithoutControls.length > 0 || propsGivenToElement.length > 0 ? (
          <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
            <div>
              <Subdued>{`Props: ${propsGivenToElement.join(', ')}${
                propsUsedWithoutControls.length > 0 ? ', ' : '.'
              }`}</Subdued>
              <VerySubdued>{`${propsUsedWithoutControls.join(', ')}${
                propsUsedWithoutControls.length > 0 ? '.' : ''
              }`}</VerySubdued>
            </div>
          </UIGridRow>
        ) : null}
      </>
    )
  },
)

export interface ComponentSectionState {
  errorOccurred: boolean
}

export class ComponentSection extends React.Component<
  ComponentSectionProps,
  ComponentSectionState
> {
  constructor(props: ComponentSectionProps) {
    super(props)
    this.state = { errorOccurred: false }
  }

  static getDerivedStateFromError(error: Error): ComponentSectionState {
    return {
      errorOccurred: true,
    }
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo): void {
    console.error('Error occurred in component section.', error, errorInfo)
  }

  render() {
    if (this.state.errorOccurred) {
      return (
        <>
          <InspectorSectionHeader>Component props</InspectorSectionHeader>

          <PropertyRow
            style={{
              gridTemplateColumns: '2fr 4fr',
            }}
          >
            <span style={{ paddingTop: 4, color: colorTheme.errorForeground.value }}>
              Invalid propertyControls value
            </span>
          </PropertyRow>
        </>
      )
    } else {
      return <ComponentSectionInner {...this.props} />
    }
  }
}
