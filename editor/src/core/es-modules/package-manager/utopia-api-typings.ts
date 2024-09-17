export const utopiaApiTypings = `declare module 'utopia-api/core' {
  export * from 'utopia-api/layout/frame';
  export * from 'utopia-api/layout/layout';
  export * from 'utopia-api/layout/flex';
  export * from 'utopia-api/helpers/helper-functions';
  export * from 'utopia-api/property-controls/property-controls';
  export * from 'utopia-api/property-controls/factories';

}
declare module 'utopia-api/helpers/helper-functions' {
  import { PropertyControls } from 'utopia-api/property-controls/property-controls';
  export interface ComponentInsertOption {
      code: string;
      additionalImports?: string;
      label?: string;
  }
  export interface ComponentToRegister {
      properties: PropertyControls;
      variants: Array<ComponentInsertOption>;
  }
  export type RawSingleBorderWidth = number | string;
  export type RawSplitBorderWidth = [
      RawSingleBorderWidth,
      RawSingleBorderWidth,
      RawSingleBorderWidth,
      RawSingleBorderWidth
  ];
  export type RawBorderWidth = RawSingleBorderWidth | RawSplitBorderWidth;
  export interface ShadowAndBorderParams {
      boxShadow?: string;
      borderStyle?: 'solid' | 'none';
      borderWidth?: RawBorderWidth;
      borderColor?: string;
  }
  export const KrazyGeorgeTestUrl = "testAsset/krazyGeorge.jpg";
  export function cssBorderToBoxShadowString(borderStyle?: 'solid' | 'none', borderWidth?: RawBorderWidth, borderColor?: string): string;
  export const UtopiaUtils: {
      shadowAndBorder(params: ShadowAndBorderParams): string;
      disabled(p: any): undefined;
  };

}
declare module 'utopia-api/helpers/helper-functions.spec' {
  export {};

}
declare module 'utopia-api/helpers/test-assets' {
  export const testImage = "data:image/jpeg;base64,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";

}
declare module 'utopia-api/index' {
  export * from 'utopia-api/primitives/common';
  export * from 'utopia-api/primitives/view';
  export * from 'utopia-api/primitives/flex-views';
  export * from 'utopia-api/primitives/rectangle';
  export * from 'utopia-api/primitives/ellipse';
  export * from 'utopia-api/primitives/scene';
  export * from 'utopia-api/primitives/storyboard';
  export * from 'utopia-api/helpers/helper-functions';
  export * from 'utopia-api/property-controls/factories';

}
declare module 'utopia-api/layout/flex' {
  /// <reference types="react" />
  import { LayoutProps } from 'utopia-api/layout/layout';
  export interface Sides {
      top?: number;
      right?: number;
      bottom?: number;
      left?: number;
  }
  export function sides(top: number | undefined, right: number | undefined, bottom: number | undefined, left: number | undefined): Sides;
  export type FlexLength = number | string | undefined;
  export interface FlexElementProps {
      position?: FlexPosition;
      left?: number | string;
      top?: number | string;
      right?: number | string;
      bottom?: number | string;
      minWidth?: number | string;
      maxWidth?: number | string;
      minHeight?: number | string;
      maxHeight?: number | string;
      marginLeft?: number | string;
      marginTop?: number | string;
      marginRight?: number | string;
      marginBottom?: number | string;
      alignSelf?: FlexAlignment;
      flexGrow?: number;
      flexShrink?: number;
      flexBasis?: FlexLength;
      crossBasis?: FlexLength;
  }
  export function flexElementPropsToStyle(props: FlexElementProps, parentProps: FlexParentProps, index: number, siblingsCount: number): Partial<React.CSSProperties>;
  type Axis = 'horizontal' | 'vertical';
  export function getMainAxis(props: FlexParentProps): Axis;
  export function getCrossAxis(props: FlexParentProps): Axis;
  export const flexWidthValueToUse: (crossBasis: FlexLength, stretches: boolean, crossAxis: Axis) => FlexLength;
  export const flexHeightValueToUse: (crossBasis: FlexLength, stretches: boolean, crossAxis: Axis) => FlexLength;
  export function getUnstretchedWidthHeight(props: LayoutProps, parentProps: FlexParentProps): {
      width: FlexLength;
      height: FlexLength;
  };
  export function getFlexSize(props: FlexElementProps, parentProps: FlexParentProps): Partial<React.CSSProperties>;
  export function getTLBRProps(props: FlexElementProps): Partial<React.CSSProperties>;
  export function getMarginProps(props: FlexElementProps, parentProps: FlexParentProps, index: number, siblingsCount: number): Partial<React.CSSProperties>;
  export interface FlexParentProps {
      flexDirection?: FlexDirection;
      alignContent?: FlexJustifyContent;
      alignItems?: FlexAlignment;
      justifyContent?: FlexJustifyContent;
      flexWrap?: FlexWrap;
      gapMain?: number;
      gapCross?: number;
      paddingLeft?: string | number;
      paddingTop?: string | number;
      paddingRight?: string | number;
      paddingBottom?: string | number;
  }
  export function flexParentPropsToStyle(props: FlexParentProps): Partial<React.CSSProperties>;
  export enum FlexAlignment {
      Auto = "auto",
      FlexStart = "flex-start",
      Center = "center",
      FlexEnd = "flex-end",
      Stretch = "stretch"
  }
  export const AllFlexAlignments: Array<FlexAlignment>;
  export enum FlexJustifyContent {
      FlexStart = "flex-start",
      Center = "center",
      FlexEnd = "flex-end",
      SpaceAround = "space-around",
      SpaceBetween = "space-between",
      SpaceEvenly = "space-evenly"
  }
  export const AllFlexJustifyContents: Array<FlexJustifyContent>;
  export enum FlexDirection {
      Column = "column",
      ColumnReverse = "column-reverse",
      Row = "row",
      RowReverse = "row-reverse"
  }
  export const AllFlexDirections: Array<FlexDirection>;
  export enum FlexWrap {
      NoWrap = "nowrap",
      Wrap = "wrap",
      WrapReverse = "wrap-reverse"
  }
  export const AllFlexWraps: Array<FlexWrap>;
  export type FlexPosition = 'absolute' | 'relative';
  export type FlexStretch = 'none' | 'horizontal' | 'vertical';
  export function getFlexStretchForChild(parent: FlexParentProps, child: FlexElementProps): FlexStretch;
  export {};

}
declare module 'utopia-api/layout/flex.spec' {
  export {};

}
declare module 'utopia-api/layout/frame' {
  interface Size {
      width: number;
      height: number;
  }
  export enum FramePoint {
      Left = "left",
      Right = "right",
      CenterX = "centerX",
      Width = "width",
      Top = "top",
      Bottom = "bottom",
      CenterY = "centerY",
      Height = "height"
  }
  export function isFramePoint(s: string): s is FramePoint;
  export const HorizontalFramePoints: FramePoint[];
  export const HorizontalFramePointsExceptSize: FramePoint[];
  export const VerticalFramePoints: FramePoint[];
  export const VerticalFramePointsExceptSize: FramePoint[];
  export const AllFramePoints: FramePoint[];
  export const AllFramePointsExceptSize: FramePoint[];
  export type FramePoints = {
      [framePoint: string]: number;
  };
  export type FramePin = string | number;
  export function isPercentPin(pin: FramePin): boolean;
  export function numberPartOfPin(pin: FramePin): number;
  export interface Frame {
      left?: FramePin;
      right?: FramePin;
      centerX?: FramePin;
      width?: FramePin;
      top?: FramePin;
      bottom?: FramePin;
      centerY?: FramePin;
      height?: FramePin;
  }
  export interface PinFrameProps {
      left?: number;
      right?: number;
      centerX?: number;
      width?: number;
      top?: number;
      bottom?: number;
      centerY?: number;
      height?: number;
  }
  export interface NormalisedFrame {
      left: number;
      top: number;
      width: number;
      height: number;
  }
  export function toNormalisedFrame(frame: Frame, parentFrame: NormalisedFrame): NormalisedFrame;
  export function toAbsoluteFrame(normalisedFrame: NormalisedFrame, parentFrame: NormalisedFrame): NormalisedFrame;
  export function zeroIfNegative(n: number): number;
  export function referenceParentValueForProp(prop: FramePoint, parentSize: Size): number;
  export function isHorizontalPoint(point: FramePoint): boolean;
  export function valueToUseForPin(prop: FramePoint, absoluteValue: number, pinIsPercentPin: boolean, parentRect: Size): string | number;
  export {};

}
declare module 'utopia-api/layout/frame.spec' {
  export {};

}
declare module 'utopia-api/layout/layout' {
  import { FlexParentProps, FlexElementProps, FlexPosition, FlexAlignment, FlexDirection, FlexJustifyContent, FlexWrap, Sides, FlexLength } from 'utopia-api/layout/flex';
  import { Frame, FramePin } from 'utopia-api/layout/frame';
  export enum LayoutSystem {
      PinSystem = "pinSystem",
      Group = "group"
  }
  export interface LayoutProps extends Frame, FlexElementProps, FlexParentProps {
      layoutSystem?: LayoutSystem;
  }
  export interface LayoutBaseProps {
      left?: FramePin;
      right?: FramePin;
      width?: FramePin;
      top?: FramePin;
      bottom?: FramePin;
      height?: FramePin;
      position?: FlexPosition;
      minWidth?: number;
      maxWidth?: number;
      minHeight?: number;
      maxHeight?: number;
      margin?: Partial<Sides>;
      alignSelf?: FlexAlignment;
      flex?: number;
      flexGrow?: number;
      flexShrink?: number;
      flexDirection?: FlexDirection;
      alignContent?: FlexJustifyContent;
      alignItems?: FlexAlignment;
      justifyContent?: FlexJustifyContent;
      wrap?: FlexWrap;
      padding?: Sides;
  }
  export interface LayoutMagicProps {
      layoutSystem?: LayoutSystem;
      centerX?: FramePin;
      centerY?: FramePin;
      flexBasis?: FlexLength;
      crossBasis?: FlexLength;
      gapCross?: number;
      gapMain?: number;
  }
  export const AllLayoutBasePropsKeys: string[];

}
declare module 'utopia-api/layout/pins' {
  /// <reference types="react" />
  import { PinFrameProps } from 'utopia-api/layout/frame';
  export interface AxisCSSProps {
      start?: number | string;
      end?: number | string;
      size?: number | string;
  }
  export function axisPinsToAxisCSSProps(start: number | string | undefined, center: number | string | undefined, end: number | string | undefined, size: number | string | undefined): AxisCSSProps;
  export type CSSFrame = Partial<Pick<React.CSSProperties, 'position' | 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'>>;
  export function convertPinsToStyleProps(props: PinFrameProps & Pick<React.CSSProperties, 'position'>): CSSFrame;
  export function convertPinsToAbsoluteStyleProps(props: PinFrameProps): CSSFrame;

}
declare module 'utopia-api/layout/pins.spec' {
  export {};

}
declare module 'utopia-api/primitives/common' {
  import React from 'react';
  export interface UtopiaComponentProps {
      'data-uid'?: string;
      'data-label'?: string;
      style?: React.CSSProperties;
  }
  export function addEventHandlersToDivProps(props: React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>): React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>;

}
declare module 'utopia-api/primitives/ellipse' {
  import React from 'react';
  import { UtopiaComponentProps } from 'utopia-api/primitives/common';
  export interface EllipseProps extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>, UtopiaComponentProps {
  }
  export const Ellipse: React.FunctionComponent<EllipseProps>;

}
declare module 'utopia-api/primitives/flex-views' {
  import { Interpolation, Theme } from '@emotion/react';
  import React from 'react';
  import { UtopiaComponentProps } from 'utopia-api/primitives/common';
  export interface FlexRowProps extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>, UtopiaComponentProps {
      css: Interpolation<Theme>;
  }
  export const FlexRow: React.FunctionComponent<FlexRowProps>;
  type FlexColProps = FlexRowProps;
  export const FlexCol: React.FunctionComponent<FlexColProps>;
  export {};

}
declare module 'utopia-api/primitives/rectangle' {
  import React from 'react';
  import { UtopiaComponentProps } from 'utopia-api/primitives/common';
  export interface RectangleProps extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>, UtopiaComponentProps {
  }
  export const Rectangle: React.FunctionComponent<RectangleProps>;

}
declare module 'utopia-api/primitives/scene' {
  import React from 'react';
  export interface SceneProps {
      style?: React.CSSProperties;
      'data-label'?: string;
      'data-uid'?: string;
  }
  export const Scene: React.MemoExoticComponent<(props: React.PropsWithChildren<SceneProps>) => JSX.Element>;

}
declare module 'utopia-api/primitives/storyboard' {
  import React from 'react';
  export const Storyboard: React.MemoExoticComponent<(props: React.PropsWithChildren<any>) => JSX.Element>;

}
declare module 'utopia-api/primitives/view' {
  import { Interpolation, Theme } from '@emotion/react';
  import React from 'react';
  import { UtopiaComponentProps } from 'utopia-api/primitives/common';
  export interface ViewProps extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>, UtopiaComponentProps {
      css?: Interpolation<Theme>;
  }
  export const View: React.FunctionComponent<ViewProps>;

}
declare module 'utopia-api/property-controls/factories' {
  import { ArrayControlDescription, BasicControlOptions, CheckboxControlDescription, ColorControlDescription, EulerControlDescription, ExpressionControlOption, ExpressionInputControlDescription, ExpressionPopUpListControlDescription, ImportType, Matrix3ControlDescription, Matrix4ControlDescription, NoneControlDescription, NumberInputControlDescription, ObjectControlDescription, PopUpListControlDescription, PropertyControls, RadioControlDescription, RegularControlDescription, StringInputControlDescription, StyleControlsControlDescription, TupleControlDescription, UnionControlDescription, Vector2ControlDescription, Vector3ControlDescription, Vector4ControlDescription } from 'utopia-api/property-controls/property-controls';
  export function checkboxControl(): CheckboxControlDescription;
  export function colorControl(): ColorControlDescription;
  export function expressionControl(): ExpressionInputControlDescription;
  export function importStar(source: string, name: string): ImportType;
  export function importDefault(source: string, name: string): ImportType;
  export function importNamed(source: string, name: string): ImportType;
  export function expression<T>(value: T, expressionString: string, requiredImport?: ImportType): ExpressionControlOption<T>;
  export function expressionPopupListControl(options: ExpressionControlOption<unknown>[]): ExpressionPopUpListControlDescription;
  export function eulerControl(): EulerControlDescription;
  export function matrix3Control(): Matrix3ControlDescription;
  export function matrix4Control(): Matrix4ControlDescription;
  export function noControl(): NoneControlDescription;
  export function numberControl(unit?: string): NumberInputControlDescription;
  export function popupListControl(options: BasicControlOptions<unknown>): PopUpListControlDescription;
  export function radioControl(options: BasicControlOptions<unknown>): RadioControlDescription;
  export function sliderControl(min: number, max: number, step: number, unit?: string): NumberInputControlDescription;
  export function stringControl(placeholder?: string): StringInputControlDescription;
  export function styleControl(): StyleControlsControlDescription;
  export function vector2Control(): Vector2ControlDescription;
  export function vector3Control(): Vector3ControlDescription;
  export function vector4Control(): Vector4ControlDescription;
  export function arrayControl(propertyControl: RegularControlDescription): ArrayControlDescription;
  export function fixedSizeArrayControl(propertyControl: RegularControlDescription, maxCount: number): ArrayControlDescription;
  export function objectControl(object: {
      [prop: string]: RegularControlDescription;
  }): ObjectControlDescription;
  export function tupleControl(propertyControls: RegularControlDescription[]): TupleControlDescription;
  export function unionControl(controls: Array<RegularControlDescription>): UnionControlDescription;

}
declare module 'utopia-api/property-controls/property-controls' {
  import type { CSSProperties } from 'react';
  export type BaseControlType = 'checkbox' | 'color' | 'euler' | 'expression-input' | 'expression-popuplist' | 'matrix3' | 'matrix4' | 'none' | 'number-input' | 'popuplist' | 'radio' | 'string-input' | 'style-controls' | 'vector2' | 'vector3' | 'vector4';
  export interface CheckboxControlDescription {
      control: 'checkbox';
      label?: string;
      visibleByDefault?: boolean;
      disabledTitle?: string;
      enabledTitle?: string;
  }
  export interface ColorControlDescription {
      control: 'color';
      label?: string;
      visibleByDefault?: boolean;
  }
  export type AllowedEnumType = string | boolean | number | undefined | null;
  export interface BasicControlOption<T> {
      value: T;
      label: string;
  }
  export type BasicControlOptions<T> = AllowedEnumType[] | BasicControlOption<T>[];
  export interface PopUpListControlDescription {
      control: 'popuplist';
      label?: string;
      visibleByDefault?: boolean;
      options: BasicControlOptions<unknown>;
  }
  export interface ImportType {
      source: string;
      name: string | null;
      type: 'star' | 'default' | null;
  }
  export interface ExpressionControlOption<T> {
      value: T;
      expression: string;
      label?: string;
      requiredImport?: ImportType;
  }
  export interface ExpressionPopUpListControlDescription {
      control: 'expression-popuplist';
      label?: string;
      visibleByDefault?: boolean;
      options: ExpressionControlOption<unknown>[];
  }
  export interface EulerControlDescription {
      control: 'euler';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface NoneControlDescription {
      control: 'none';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface Matrix3ControlDescription {
      control: 'matrix3';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface Matrix4ControlDescription {
      control: 'matrix4';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface NumberInputControlDescription {
      control: 'number-input';
      label?: string;
      visibleByDefault?: boolean;
      max?: number;
      min?: number;
      unit?: string;
      step?: number;
      displayStepper?: boolean;
  }
  export interface RadioControlDescription {
      control: 'radio';
      label?: string;
      visibleByDefault?: boolean;
      options: BasicControlOptions<unknown>;
  }
  export interface ExpressionInputControlDescription {
      control: 'expression-input';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface StringInputControlDescription {
      control: 'string-input';
      label?: string;
      visibleByDefault?: boolean;
      placeholder?: string;
      obscured?: boolean;
  }
  export interface StyleControlsControlDescription {
      control: 'style-controls';
      label?: string;
      visibleByDefault?: boolean;
      placeholder?: CSSProperties;
  }
  export interface Vector2ControlDescription {
      control: 'vector2';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface Vector3ControlDescription {
      control: 'vector3';
      label?: string;
      visibleByDefault?: boolean;
  }
  export interface Vector4ControlDescription {
      control: 'vector4';
      label?: string;
      visibleByDefault?: boolean;
  }
  export type BaseControlDescription = CheckboxControlDescription | ColorControlDescription | ExpressionInputControlDescription | ExpressionPopUpListControlDescription | EulerControlDescription | NoneControlDescription | Matrix3ControlDescription | Matrix4ControlDescription | NumberInputControlDescription | RadioControlDescription | PopUpListControlDescription | StringInputControlDescription | StyleControlsControlDescription | Vector2ControlDescription | Vector3ControlDescription | Vector4ControlDescription;
  export type HigherLevelControlType = 'array' | 'tuple' | 'object' | 'union';
  export type RegularControlType = BaseControlType | HigherLevelControlType;
  export type ControlType = RegularControlType | 'folder';
  export interface ArrayControlDescription {
      control: 'array';
      label?: string;
      visibleByDefault?: boolean;
      propertyControl: RegularControlDescription;
      maxCount?: number;
  }
  export interface ObjectControlDescription {
      control: 'object';
      label?: string;
      visibleByDefault?: boolean;
      object: {
          [prop: string]: RegularControlDescription;
      };
  }
  export interface UnionControlDescription {
      control: 'union';
      label?: string;
      visibleByDefault?: boolean;
      controls: Array<RegularControlDescription>;
  }
  export interface TupleControlDescription {
      control: 'tuple';
      label?: string;
      visibleByDefault?: boolean;
      propertyControls: RegularControlDescription[];
  }
 
  export type HigherLevelControlDescription = ArrayControlDescription | ObjectControlDescription | TupleControlDescription | UnionControlDescription;
  export type RegularControlDescription = BaseControlDescription | HigherLevelControlDescription;
  export type ControlDescription = RegularControlDescription;
  export function isBaseControlDescription(control: ControlDescription): control is BaseControlDescription;
  export function isHigherLevelControlDescription(control: ControlDescription): control is HigherLevelControlDescription;
  export type PropertyControls = {
      [key: string]: ControlDescription;
  };
  export function addPropertyControls(component: unknown, propertyControls: PropertyControls): void;

}
declare module 'utopia-api/tests/test-utils' {
  /// <reference types="react" />
  export const flexRowStyle: Pick<React.CSSProperties, 'display' | 'flexDirection' | 'alignItems' | 'whiteSpace'>;
  export const flexColumnStyle: Pick<React.CSSProperties, 'display' | 'flexDirection' | 'alignItems' | 'whiteSpace'>;
  interface CommonSenseUtopiaProps {
      flexGrow?: number;
  }
  export const commonSenseUtopiaLayoutShorthands: (props: CommonSenseUtopiaProps) => import("@emotion/react").SerializedStyles;
  export const FlexRow: import("@emotion/styled").StyledComponent<{
      theme?: import("@emotion/react").Theme | undefined;
      as?: import("react").ElementType<any> | undefined;
  } & CommonSenseUtopiaProps, import("react").DetailedHTMLProps<import("react").HTMLAttributes<HTMLDivElement>, HTMLDivElement>, {}>;
  export const FlexColumn: import("@emotion/styled").StyledComponent<{
      theme?: import("@emotion/react").Theme | undefined;
      as?: import("react").ElementType<any> | undefined;
  } & CommonSenseUtopiaProps, import("react").DetailedHTMLProps<import("react").HTMLAttributes<HTMLDivElement>, HTMLDivElement>, {}>;
  export {};

}
declare module 'utopia-api/utils' {
  export function defaultIfNull<T>(defaultValue: T, value: T | null | undefined): T;
  export function fastForEach<T>(a: readonly T[], fn: (t: T, index: number) => void): void;

}
declare module 'utopia-api' {
  import main = require('utopia-api/index');
  export = main;
}`
