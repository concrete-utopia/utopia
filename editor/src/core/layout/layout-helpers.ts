import {
  FlexAlignment,
  FlexElementProps,
  FlexLength,
  FlexStretch,
  FramePoint,
  getUnstretchedWidthHeight,
  LayoutSystem,
  UtopiaComponentProps,
} from 'utopia-api'
import { FullFrame } from '../../components/frame'
import {
  applicative2Either,
  defaultEither,
  Either,
  flatMapEither,
  foldEither,
  isRight,
  joinEither,
  left,
  mapEither,
  reduceWithEither,
  right,
  eitherToMaybe,
} from '../shared/either'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXAttributes,
  jsxAttributeValue,
  JSXElement,
  ComponentMetadata,
  UtopiaJSXComponent,
} from '../shared/element-template'
import {
  setJSXValueAtPath,
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
} from '../shared/jsx-attributes'
import { Imports, PropertyPath, TemplatePath } from '../shared/project-file-types'
import { createLayoutPropertyPath, pinnedPropForFramePoint } from './layout-helpers-new'
import { getLayoutProperty, getLayoutPropertyOr } from './getLayoutProperty'
import { PropsOrJSXAttributes, getSimpleAttributeAtPath } from '../model/element-metadata-utils'
import { EdgePosition } from '../../components/canvas/canvas-types'
import { EditorState } from '../../components/editor/store/editor-state'
import { getPropertyControlsForTarget } from '../property-controls/property-controls-utils'
import { PropertyControlsInfo } from '../../components/custom-code/code-file'

export function targetRespectsLayout(
  target: TemplatePath,
  propertyControlsInfo: PropertyControlsInfo,
  openImports: Imports,
  openFilePath: string | null,
  rootComponents: UtopiaJSXComponent[],
  jsxMetadataKILLME: ComponentMetadata[],
): boolean {
  const propControls = getPropertyControlsForTarget(
    target,
    propertyControlsInfo,
    openImports,
    openFilePath,
    rootComponents,
    jsxMetadataKILLME,
  )
  return propControls?.style?.type === 'styleobject'
}

export const PinLayoutHelpers = {
  setLayoutPropsToPinsWithFrame(
    props: JSXAttributes,
    frame: Partial<FullFrame>,
  ): Either<string, JSXAttributes> {
    return reduceWithEither(
      (workingProps, frameProp: FramePoint) => {
        return setJSXValueAtPath(
          workingProps,
          createLayoutPropertyPath(pinnedPropForFramePoint(frameProp)),
          jsxAttributeValue(frame[frameProp]),
        )
      },
      props,
      Object.keys(frame) as FramePoint[],
    )
  },
  updateLayoutPropsWithFrame(
    props: JSXAttributes,
    frame: Partial<FullFrame>,
  ): Either<string, JSXAttributes> {
    // like setLayoutPropsToPinsWithFrame, but preserves the original layout props
    return reduceWithEither(
      (updatedAttributes, frameProp) => {
        return setJSXValueAtPath(
          updatedAttributes,
          createLayoutPropertyPath(pinnedPropForFramePoint(frameProp as FramePoint)),
          jsxAttributeValue(frame[frameProp]),
        )
      },
      props,
      Object.keys(frame) as Array<keyof Partial<FullFrame>>,
    )
  },
}

export type FixedAndBases = {
  flexBasis: number | null
  crossBasis: number | null
}

export type TopLeftWidthHeight = Pick<FullFrame, 'top' | 'left' | 'width' | 'height'>

export const FlexBasisPath = createLayoutPropertyPath('FlexFlexBasis')

export const CrossBasisPath = createLayoutPropertyPath('FlexCrossBasis')

export const FlexLayoutHelpers = {
  updateLayoutPropsToFlexWithFrame(
    parentProps: PropsOrJSXAttributes,
    elementProps: JSXAttributes,
    width: number,
    height: number,
  ): Either<string, JSXAttributes> {
    // Remove all of the properties so that old values don't mess with this.
    const withoutLayoutProps = unsetJSXValuesAtPaths(elementProps, [FlexBasisPath, CrossBasisPath])

    return joinEither(
      applicative2Either(
        (props, { flexBasis, crossBasis }) => {
          // Build the new properties.
          let propsToSet: Array<ValueAtPath> = []
          function addPropToSet(path: PropertyPath, value: string | number | undefined): void {
            if (value != null) {
              propsToSet.push({ path: path, value: jsxAttributeValue(value) })
            }
          }
          if (flexBasis != null) {
            addPropToSet(FlexBasisPath, flexBasis)
          }
          if (crossBasis != null) {
            addPropToSet(CrossBasisPath, crossBasis)
          }

          // Assign the new properties
          return setJSXValuesAtPaths(props, propsToSet)
        },
        withoutLayoutProps,
        FlexLayoutHelpers.convertWidthHeightToFlex(width, height, elementProps, parentProps, null),
      ),
    )
  },
  getFlexDirectionFromProps(
    props: JSXAttributes,
  ): 'row' | 'row-reverse' | 'column' | 'column-reverse' {
    return getLayoutPropertyOr(
      'row', // TODO read this value from spy
      'flexDirection',
      right(props),
    )
  },
  getFlexWrap: function (props: JSXAttributes): 'wrap' | 'wrap-reverse' | 'nowrap' {
    return getLayoutPropertyOr(
      'nowrap', // TODO read this value from spy
      'flexWrap',
      right(props),
    )
  },
  setTopLeftAttributes: (pos: { top: number; left: number }) => (props: JSXAttributes) => {
    return reduceWithEither(
      (workingProps, propToSet) => {
        return setJSXValueAtPath(
          workingProps,
          createLayoutPropertyPath(propToSet),
          jsxAttributeValue(pos[propToSet]),
        )
      },
      props,
      ['top', 'left'] as const,
    )
  },
  setWidthHeightAttributes: (size: { width: number; height: number }) => (props: JSXAttributes) => {
    return reduceWithEither(
      (workingProps, propToSet) => {
        return setJSXValueAtPath(
          workingProps,
          createLayoutPropertyPath(pinnedPropForFramePoint(propToSet)),
          jsxAttributeValue(size[propToSet]),
        )
      },
      props,
      [FramePoint.Width, FramePoint.Height] as const,
    )
  },
  setFlexAlignSelf: (newValue: FlexAlignment) => (
    props: JSXAttributes,
  ): Either<string, JSXAttributes> => {
    return setJSXValueAtPath(
      props,
      createLayoutPropertyPath('alignSelf'),
      jsxAttributeValue(newValue),
    )
  },
  getMainAxis(props: PropsOrJSXAttributes): Either<string, 'horizontal' | 'vertical'> {
    return mapEither((fd) => {
      if (fd === 'column' || fd === 'column-reverse') {
        return 'vertical'
      } else {
        return 'horizontal'
      }
    }, getSimpleAttributeAtPath(props, createLayoutPropertyPath('flexDirection')))
  },
  getCrossAxis(props: PropsOrJSXAttributes): Either<string, 'horizontal' | 'vertical'> {
    return mapEither(
      (fd) => (fd === 'vertical' ? 'horizontal' : 'vertical'),
      this.getMainAxis(props),
    )
  },
  convertWidthHeightToFlex: (
    width: number,
    height: number,
    props: JSXAttributes,
    parentProps: PropsOrJSXAttributes,
    edgePosition: EdgePosition | null,
  ): Either<string, FixedAndBases> => {
    const possibleMainAxis = FlexLayoutHelpers.getMainAxis(parentProps)
    const currentFlexBasis = eitherToMaybe(getSimpleAttributeAtPath(right(props), FlexBasisPath))
    const currentCrossBasis = eitherToMaybe(getSimpleAttributeAtPath(right(props), CrossBasisPath))
    return mapEither((mainAxis) => {
      const flexBasis = mainAxis === 'horizontal' ? width : height
      const crossBasis = mainAxis === 'vertical' ? width : height
      let shouldSetFlexBasis = true
      let shouldSetCrossBasis = true
      if (edgePosition != null) {
        shouldSetFlexBasis =
          mainAxis === 'horizontal' ? edgePosition.y === 0.5 : edgePosition.x === 0.5
        shouldSetCrossBasis =
          mainAxis === 'vertical' ? edgePosition.y === 0.5 : edgePosition.x === 0.5
      }
      return {
        flexBasis: currentFlexBasis != null || shouldSetFlexBasis ? flexBasis : null,
        crossBasis: currentCrossBasis != null || shouldSetCrossBasis ? crossBasis : null,
      }
    }, possibleMainAxis)
  },
  getUnstretchedWidthHeight: (
    props: UtopiaComponentProps,
    parentProps: UtopiaComponentProps,
  ): { width: FlexLength; height: FlexLength } => {
    return getUnstretchedWidthHeight(props.layout || {}, parentProps.layout || {})
  },
}

export const LayoutHelpers = {
  updateLayoutPropsWithFrame(
    parentIsFlex: boolean,
    parentProps: PropsOrJSXAttributes | null,
    elementProps: JSXAttributes,
    frame: TopLeftWidthHeight,
  ): Either<string, JSXAttributes> {
    if (parentIsFlex && parentProps != null) {
      return FlexLayoutHelpers.updateLayoutPropsToFlexWithFrame(
        parentProps,
        elementProps,
        frame.width,
        frame.height,
      )
    } else {
      return PinLayoutHelpers.updateLayoutPropsWithFrame(elementProps, frame)
    }
  },
  getLayoutSystemFromAttributes(props: PropsOrJSXAttributes): Either<string, LayoutSystem> {
    return getSimpleAttributeAtPath(props, createLayoutPropertyPath('LayoutSystem')) // TODO LAYOUT investigate if we should use the DOM walker results here
  },
  getLayoutSystemFromProps(props: PropsOrJSXAttributes): LayoutSystem {
    const parsedLayoutSystem = this.getLayoutSystemFromAttributes(props)
    return isRight(parsedLayoutSystem) ? parsedLayoutSystem.value : LayoutSystem.PinSystem
  },
  setLayoutAttribute: (newValue: LayoutSystem) => (
    props: JSXAttributes,
  ): Either<string, JSXAttributes> => {
    return setJSXValueAtPath(
      props,
      createLayoutPropertyPath('LayoutSystem'),
      jsxAttributeValue(newValue),
    )
  },
  stretchesChild(
    parentProps: PropsOrJSXAttributes,
    childProps: JSXAttributes,
  ): Either<string, boolean> {
    return applicative2Either(
      (alignSelf, alignItems) => {
        const childrenStretchesSelf = alignSelf === 'stretch'
        const childrenAlignsSelfAuto = alignSelf === 'auto' || alignSelf == null
        const parentStretchesChildren = alignItems === 'stretch'
        return childrenStretchesSelf || (parentStretchesChildren && childrenAlignsSelfAuto)
      },
      getLayoutProperty('alignSelf', right(childProps)),
      getLayoutProperty('alignItems', parentProps),
    )
  },
  getFlexStretchForChild(
    parent: Either<ComponentMetadata, ElementInstanceMetadata>,
    child: ElementInstanceMetadata,
  ): Either<string, FlexStretch> {
    const parentScenePropsOrElementAttributes: PropsOrJSXAttributes | null = foldEither(
      (parentScene) => left(parentScene.globalFrame),
      (elementMetadata) => {
        return foldEither(
          (_) => null,
          (success) => {
            if (isJSXElement(success)) {
              return right(success.props)
            } else {
              return null
            }
          },
          elementMetadata.element,
        )
      },
      parent,
    )
    if (parentScenePropsOrElementAttributes == null) {
      return left('Unknown parent attributes.')
    } else {
      if (isRight(child.element) && isJSXElement(child.element.value)) {
        const childJSXElement: JSXElement = child.element.value
        return flatMapEither((stretched) => {
          if (stretched) {
            return FlexLayoutHelpers.getCrossAxis(parentScenePropsOrElementAttributes)
          } else {
            return right('none')
          }
        }, this.stretchesChild(parentScenePropsOrElementAttributes, childJSXElement.props))
      } else {
        return left('Unknown child attributes.')
      }
    }
  },
  getElementSizePropertyPaths(
    element: ElementInstanceMetadata,
  ): {
    horizontal: PropertyPath
    vertical: PropertyPath
  } {
    if (element.specialSizeMeasurements.parentLayoutSystem === 'flex') {
      if (element.specialSizeMeasurements.parentFlexDirection === 'row') {
        return {
          horizontal: createLayoutPropertyPath('FlexFlexBasis'),
          vertical: createLayoutPropertyPath('FlexCrossBasis'),
        }
      } else {
        return {
          horizontal: createLayoutPropertyPath('FlexCrossBasis'),
          vertical: createLayoutPropertyPath('FlexFlexBasis'),
        }
      }
    } else {
      return {
        horizontal: createLayoutPropertyPath('Width'),
        vertical: createLayoutPropertyPath('Height'),
      }
    }
  },
}
