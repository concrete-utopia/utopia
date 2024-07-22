import type { ErrorMessage } from '../../../core/shared/error-messages'
import { errorMessage } from '../../../core/shared/error-messages'
import type {
  PackageDetails,
  PackageStatus,
  PackageStatusMap,
} from '../../../core/shared/npm-dependency-types'
import { packageDetails } from '../../../core/shared/npm-dependency-types'
import type {
  AssetFile,
  Directory,
  ElementPath,
  ESCodeFile,
  ESRemoteDependencyPlaceholder,
  ExportClass,
  ExportDefaultFunctionOrClass,
  ExportDestructuredAssignment,
  ExportDetail,
  ExportFunction,
  ExportDefaultIdentifier,
  ExportVariable,
  ExportVariables,
  ExportVariablesWithModifier,
  HighlightBounds,
  HighlightBoundsForUids,
  ImageFile,
  ImportAlias,
  ImportDetails,
  NodeModuleFile,
  ParsedJSONFailure,
  ParsedTextFile,
  ParseFailure,
  ParseSuccess,
  ProjectFile,
  ReexportVariables,
  ReexportWildcard,
  RevisionsStateType,
  TextFile,
  TextFileContents,
  Unparsed,
  ElementPropertyPath,
} from '../../../core/shared/project-file-types'
import { directory } from '../../../core/shared/project-file-types'
import {
  assetFile,
  esCodeFile,
  esRemoteDependencyPlaceholder,
  exportClass,
  exportDefaultFunctionOrClass,
  exportDestructuredAssignment,
  exportFunction,
  exportDefaultIdentifier,
  exportVariable,
  exportVariables,
  exportVariablesWithModifier,
  highlightBounds,
  imageFile,
  importAlias,
  importDetails,
  parseFailure,
  parseSuccess,
  reexportVariables,
  reexportWildcard,
  textFile,
  textFileContents,
} from '../../../core/shared/project-file-types'
import type {
  DetailedTypeInfo,
  DetailedTypeInfoMemberInfo,
  ExportsInfo,
  ExportType,
  MultiFileBuildResult,
  SingleFileBuildResult,
} from '../../../core/workers/common/worker-types'
import {
  detailedTypeInfo,
  detailedTypeInfoMemberInfo,
  exportsInfo,
  exportType,
  singleFileBuildResult,
} from '../../../core/workers/common/worker-types'
import type { Sides, Focus, Styling, Emphasis, Icon, InspectorSpec } from 'utopia-api/core'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  ElementsWithin,
  JSExpression,
  JSXArrayElement,
  JSXArraySpread,
  JSXArrayValue,
  JSExpressionFunctionCall,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSXAttributes,
  JSExpressionValue,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  JSXProperty,
  JSXPropertyAssignment,
  JSXSpreadAssignment,
  JSXTextBlock,
  MultiLineComment,
  SingleLineComment,
  Comment,
  SpecialSizeMeasurements,
  JSXAttributesEntry,
  ImportInfo,
  JSXAttributesSpread,
  JSXAttributesPart,
  ParsedComments,
  UtopiaJSXComponent,
  ArbitraryJSBlock,
  BoundParam,
  RegularParam,
  DestructuredObject,
  DestructuredParamPart,
  Param,
  DestructuredArray,
  DestructuredArrayPart,
  StyleAttributeMetadata,
  StyleAttributeMetadataEntry,
  TopLevelElement,
  ImportStatement,
  UnparsedCode,
  JSXElementWithoutUID,
  SameFileOrigin,
  ImportedOrigin,
  ConditionValue,
  JSXConditionalExpressionWithoutUID,
  JSXConditionalExpression,
  ActiveAndDefaultConditionValues,
  JSXMapExpression,
  JSExpressionMapOrOtherJavascript,
  JSExpressionOtherJavaScript,
  JSIdentifier,
  JSPropertyAccess,
  JSElementAccess,
  OptionallyChained,
  EarlyReturnResult,
  EarlyReturnVoid,
  JSArbitraryStatement,
  JSOpaqueArbitraryStatement,
  JSAssignmentStatement,
  JSAssignment,
  GridContainerProperties,
  GridTemplate,
  GridAuto,
  GridElementProperties,
  GridPositionValue,
  GridPosition,
  GridAutoOrTemplateBase,
  GridAutoOrTemplateDimensions,
  GridAutoOrTemplateFallback,
} from '../../../core/shared/element-template'
import {
  elementInstanceMetadata,
  isArraySpread,
  isArrayValue,
  modifiableAttributeIsAttributeFunctionCall,
  modifiableAttributeIsAttributeNestedArray,
  modifiableAttributeIsAttributeNestedObject,
  modifiableAttributeIsAttributeOtherJavaScript,
  isJSXAttributeValue,
  isJSXElement,
  isJSXFragment,
  isJSXTextBlock,
  isMultiLineComment,
  isPropertyAssignment,
  isSingleLineComment,
  isSpreadAssignment,
  jsxArraySpread,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsExpressionValue,
  jsxElement,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  multiLineComment,
  singleLineComment,
  specialSizeMeasurements,
  jsxAttributesEntry,
  jsxAttributesSpread,
  isJSXAttributesEntry,
  isJSXAttributesSpread,
  parsedComments,
  utopiaJSXComponent,
  isRegularParam,
  isDestructuredObject,
  isDestructuredArray,
  regularParam,
  isOmittedParam,
  isParam,
  destructuredObject,
  destructuredParamPart,
  destructuredArray,
  functionParam,
  importStatement,
  unparsedCode,
  jsxElementWithoutUID,
  sameFileOrigin,
  importedOrigin,
  isJSXConditionalExpression,
  jsxConditionalExpression,
  isJSExpressionOtherJavaScript,
  isJSXMapExpression,
  arbitraryJSBlock,
  jsExpressionOtherJavaScript,
  isJSExpressionValue,
  isJSExpressionNestedArray,
  isJSExpressionNestedObject,
  isJSExpressionFunctionCall,
  jsIdentifier,
  jsPropertyAccess,
  jsElementAccess,
  isJSIdentifier,
  isJSPropertyAccess,
  isJSElementAccess,
  earlyReturnResult,
  jsOpaqueArbitraryStatement,
  jsAssignmentStatement,
  jsAssignment,
  jsxMapExpression,
  gridContainerProperties,
  gridElementProperties,
  gridPositionValue,
  gridAutoOrTemplateFallback,
  gridAutoOrTemplateDimensions,
} from '../../../core/shared/element-template'
import type {
  CanvasRectangle,
  CoordinateMarker,
  LocalPoint,
  LocalRectangle,
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
  MaybeInfinityRectangle,
  Rectangle,
  Size,
} from '../../../core/shared/math-utils'
import { isInfinityRectangle, size } from '../../../core/shared/math-utils'
import type { RawSourceMap } from '../../../core/workers/ts/ts-typings/RawSourceMap'
import type { KeepDeepEqualityResult, KeepDeepEqualityCall } from '../../../utils/deep-equality'
import {
  keepDeepEqualityResult,
  combine3EqualityCalls,
  combine6EqualityCalls,
  nullableDeepEquality,
  createCallWithTripleEquals,
  objectDeepEquality,
  combine5EqualityCalls,
  arrayDeepEquality,
  combine2EqualityCalls,
  combine7EqualityCalls,
  combine8EqualityCalls,
  undefinableDeepEquality,
  combine4EqualityCalls,
  combine12EqualityCalls,
  combine1EqualityCall,
  createCallWithShallowEquals,
  combine10EqualityCalls,
  createCallWithDeepEquals,
  readOnlyArrayDeepEquality,
  StringKeepDeepEquality,
  BooleanKeepDeepEquality,
  NullableStringKeepDeepEquality,
  NumberKeepDeepEquality,
  NullableNumberKeepDeepEquality,
  combine9EqualityCalls,
  unionDeepEquality,
  combine14EqualityCalls,
  combine11EqualityCalls,
  combine15EqualityCalls,
  combine16EqualityCalls,
} from '../../../utils/deep-equality'
import {
  ElementPathArrayKeepDeepEquality,
  HigherOrderControlArrayKeepDeepEquality,
  ElementPathKeepDeepEquality,
  EitherKeepDeepEquality,
  JSXElementNameKeepDeepEqualityCall,
  ElementWarningsKeepDeepEquality,
  WindowPointKeepDeepEquality,
  CanvasPointKeepDeepEquality,
  StaticElementPathKeepDeepEquality,
  ElementsToRerenderKeepDeepEquality,
  PropertyPathKeepDeepEquality,
} from '../../../utils/deep-equality-instances'
import {
  createCallFromIntrospectiveKeepDeep,
  getIntrospectiveKeepDeepResult,
} from '../../../utils/react-performance'
import type {
  TransientFilesState,
  DerivedState,
  EditorStateNodeModules,
  EditorStateLeftMenu,
  EditorStateRightMenu,
  EditorStateInterfaceDesigner,
  EditorStateCanvasTextEditor,
  EditorStateCanvasTransientProperty,
  EditorStateCanvasControls,
  EditorStateCanvas,
  DuplicationState,
  OriginalPath,
  ImageBlob,
  UIFileBase64Blobs,
  CanvasBase64Blobs,
  DesignerFile,
  ResizeOptions,
  EditorState,
  CanvasCursor,
  CursorStackItem,
  CursorImportanceLevel,
  EditorStateInspector,
  EditorStateFileBrowser,
  EditorStateDependencyList,
  EditorStateGenericExternalResources,
  EditorStateGoogleFontsResources,
  EditorStateProjectSettings,
  EditorStateTopMenu,
  EditorStatePreview,
  EditorStateHome,
  FileDeleteModal,
  ModalDialog,
  EditorStateCodeEditorErrors,
  ErrorMessages,
  AllElementProps,
  LockedElements,
  ProjectGithubSettings,
  GithubRepo,
  DraggedImageProperties,
  ImageDragSessionState,
  DraggingFromSidebar,
  FileUploadInfo,
  FileOverwriteModal,
  GithubOperation,
  FileChecksums,
  FileRevertModal,
  GithubData,
  DragToMoveIndicatorFlags,
  ColorSwatch,
  NavigatorEntry,
  RegularNavigatorEntry,
  ConditionalClauseNavigatorEntry,
  SyntheticNavigatorEntry,
  DropTargetHint,
  NavigatorState,
  InternalClipboard,
  FileWithChecksum,
  FileChecksumsWithFile,
  PostActionMenuData,
  PastePostActionMenuData,
  PasteHerePostActionMenuData,
  PasteToReplacePostActionMenuData,
  NavigatorReparentPostActionMenuData,
  TrueUpGroupElementChanged,
  TrueUpChildrenOfGroupChanged,
  TrueUpTarget,
  InvalidOverrideNavigatorEntry,
  TrueUpHuggingElement,
  RenderPropNavigatorEntry,
  SlotNavigatorEntry,
  RenderPropValueNavigatorEntry,
  DataReferenceNavigatorEntry,
  GithubUser,
  PullRequest,
  RenderedAt,
} from './editor-state'
import {
  trueUpGroupElementChanged,
  trueUpChildrenOfGroupChanged,
  invalidOverrideNavigatorEntry,
  trueUpHuggingElement,
  renderPropNavigatorEntry,
  renderPropValueNavigatorEntry,
  dataReferenceNavigatorEntry,
  newGithubData,
  renderedAtPropertyPath,
  renderedAtChildNode,
} from './editor-state'
import {
  editorStateNodeModules,
  editorStateLeftMenu,
  editorStateRightMenu,
  editorStateInterfaceDesigner,
  editorStateCanvasTextEditor,
  editorStateCanvasTransientProperty,
  editorStateCanvasControls,
  duplicationState,
  originalPath,
  imageBlob,
  designerFile,
  resizeOptions,
  editorStateCanvas,
  cursorStackItem,
  canvasCursor,
  editorStateInspector,
  editorStateFileBrowser,
  editorStateDependencyList,
  editorStateGenericExternalResources,
  editorStateGoogleFontsResources,
  editorStateProjectSettings,
  editorStateTopMenu,
  editorStatePreview,
  editorStateHome,
  fileDeleteModal,
  fileOverwriteModal,
  editorStateCodeEditorErrors,
  editorState,
  githubRepo,
  draggedImageProperties,
  draggingFromSidebar,
  fileUploadInfo,
  fileRevertModal,
  emptyGithubData,
  dragToMoveIndicatorFlags,
  projectGithubSettings,
  newColorSwatch,
  regularNavigatorEntry,
  conditionalClauseNavigatorEntry,
  syntheticNavigatorEntry,
  internalClipboard,
} from './editor-state'
import type {
  CornerGuideline,
  XAxisGuideline,
  YAxisGuideline,
  Guideline,
  GuidelineWithSnappingVectorAndPointsOfRelevance,
} from '../../canvas/guideline'
import {
  xAxisGuideline,
  yAxisGuideline,
  cornerGuideline,
  guidelineWithSnappingVectorAndPointsOfRelevance,
} from '../../canvas/guideline'
import type {
  BoundingArea,
  CanvasControlType,
  DragInteractionData,
  FlexGapHandle,
  ReorderSlider,
  HoverInteractionData,
  InputData,
  InteractionSession,
  KeyboardCatcherControl,
  KeyboardInteractionData,
  KeyState,
  PaddingResizeHandle,
  ResizeHandle,
  BorderRadiusResizeHandle,
  ZeroDragPermitted,
  GridCellHandle,
  GridAxisHandle,
  GridResizeHandle,
  GridResizeEdge,
} from '../../canvas/canvas-strategies/interaction-state'
import {
  boundingArea,
  flexGapHandle,
  reorderSlider,
  interactionSession,
  keyboardCatcherControl,
  resizeHandle,
  gridCellHandle,
  gridAxisHandle,
  gridResizeHandle,
} from '../../canvas/canvas-strategies/interaction-state'
import type { Modifiers } from '../../../utils/modifiers'
import type {
  CanvasFrameAndTarget,
  CSSCursor,
  EdgePosition,
  FrameAndTarget,
} from '../../canvas/canvas-types'
import { edgePosition } from '../../canvas/canvas-types'
import type {
  ProjectContentDirectory,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { projectContentDirectory, projectContentFile } from '../../assets'
import { parsedJSONFailure } from '../../../core/workers/common/project-file-utils'
import type {
  CodeResult,
  CodeResultCache,
  ComponentDescriptor,
  ComponentDescriptorSource,
  ComponentDescriptorsForFile,
  ComponentElementToInsert,
  ComponentInfo,
  CurriedResolveFn,
  CurriedUtopiaRequireFn,
  PropertyControlsInfo,
  ComponentDescriptorFromDescriptorFile,
  TypedInpsectorSpec,
  ShownInspectorSpec,
  StyleSectionState,
} from '../../custom-code/code-file'
import {
  codeResult,
  codeResultCache,
  componentDescriptor,
  componentInfo,
  componentDescriptorFromDescriptorFile,
} from '../../custom-code/code-file'
import type {
  EvaluationCache,
  EvaluationCacheForPath,
  FileEvaluationCache,
} from '../../../core/es-modules/package-manager/package-manager'
import {
  evaluationCacheForPath,
  fileEvaluationCache,
} from '../../../core/es-modules/package-manager/package-manager'
import type {
  ImageInsertionSubject,
  InsertionSubject,
  InsertMode,
  IsDragging,
  LiveCanvasMode,
  Mode,
  SelectMode,
  TargetedInsertionParent,
  TextEditMode,
  Coordinates,
  TextEditableElementState,
  InsertionSubjectWrapper,
  SelectModeToolbarMode,
  CommentMode,
  FollowMode,
  CommentId,
  NewComment,
  ExistingComment,
  NewCommentLocation,
  CanvasCommentLocation,
  SceneCommentLocation,
} from '../editor-modes'
import {
  EditorModes,
  insertionSubject,
  targetedInsertionParent,
  imageInsertionSubject,
  newComment,
  existingComment,
  canvasCommentLocation,
  sceneCommentLocation,
} from '../editor-modes'
import type { EditorPanel } from '../../common/actions'
import type { Notice, NoticeLevel } from '../../common/notice'
import { notice } from '../../common/notice'
import type { Absolute, After, Back, Before, Front, IndexPosition } from '../../../utils/utils'
import { absolute, back, front, after, before } from '../../../utils/utils'
import type {
  CSSColor,
  CSSFontFamily,
  CSSFontSize,
  CSSFontWeightAndStyle,
  CSSLetterSpacing,
  CSSLineHeight,
  CSSNumber,
  CSSNumberUnit,
  CSSTextAlign,
  CSSTextDecorationLine,
  FontSettings,
  GridCSSNumber,
  GridCSSNumberUnit,
} from '../../inspector/common/css-utils'
import { cssNumber, fontSettings, gridCSSNumber } from '../../inspector/common/css-utils'
import type { ElementPaste, ProjectListing } from '../action-types'
import { projectListing } from '../action-types'
import type { Bounds, UtopiaVSCodeConfig } from 'utopia-vscode-common'
import type { MouseButtonsPressed } from '../../../utils/mouse'
import { assertNever } from '../../../core/shared/utils'
import type {
  AssetResult,
  ImageResult,
  FileResult,
  TextResult,
} from '../../../core/shared/file-utils'
import { assetResult, imageResult, textResult } from '../../../core/shared/file-utils'
import type {
  GithubBranch,
  GithubFileChanges,
  GithubFileStatus,
  RepositoryEntry,
  RepositoryEntryPermissions,
} from '../../../core/shared/github/helpers'
import {
  emptyGithubFileChanges,
  repositoryEntry,
  repositoryEntryPermissions,
} from '../../../core/shared/github/helpers'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { valueAtPath } from '../../../core/shared/jsx-attributes'
import type { ConditionalCase } from '../../../core/model/conditionals'
import type {
  ChildInsertionPath,
  ConditionalClauseInsertBehavior,
  ConditionalClauseInsertionPath,
  InsertionPath,
} from './insertion-path'
import { childInsertionPath, conditionalClauseInsertionPath } from './insertion-path'
import type { ElementPathTree, ElementPathTrees } from '../../../core/shared/element-path-tree'
import { elementPathTree } from '../../../core/shared/element-path-tree'
import type { CopyData, ElementPasteWithMetadata } from '../../../utils/clipboard'
import { elementPaste } from '../actions/action-creators'
import type { ProjectMetadataFromServer, ProjectServerState } from './project-server-state'
import { projectServerState, projectMetadataFromServer } from './project-server-state'
import type { VariablesInScope } from '../../canvas/ui-jsx-canvas'
import type {
  ActiveFrame,
  ActiveFrameTarget,
  ActiveFrameTargetPath,
  ActiveFrameTargetRect,
} from '../../canvas/commands/set-active-frames-command'
import type { CommentFilterMode } from '../../inspector/sections/comment-section'
import type { Collaborator } from '../../../core/shared/multiplayer'
import type { MultiplayerSubstate } from './store-hook-substore-types'
import type {
  PreferredChildComponentDescriptor,
  PropertyControls,
} from '../../custom-code/internal-property-controls'
import type { OnlineState } from '../online-status'
import { onlineState } from '../online-status'
import type { NavigatorRow } from '../../navigator/navigator-row'
import { condensedNavigatorRow, regularNavigatorRow } from '../../navigator/navigator-row'
import type { SimpleFunctionWrap, FunctionWrap } from 'utopia-shared/src/types'
import { simpleFunctionWrap, isSimpleFunctionWrap } from 'utopia-shared/src/types'
import type {
  ComponentDescriptorBounds,
  ComponentDescriptorPropertiesBounds,
} from '../../../core/property-controls/component-descriptor-parser'

export function ElementPropertyPathKeepDeepEquality(): KeepDeepEqualityCall<ElementPropertyPath> {
  return combine2EqualityCalls(
    (e) => e.elementPath,
    ElementPathKeepDeepEquality,
    (e) => e.propertyPath,
    PropertyPathKeepDeepEquality(),
    (elementPath, propertyPath) => ({ elementPath, propertyPath }),
  )
}

export const ProjectMetadataFromServerKeepDeepEquality: KeepDeepEqualityCall<ProjectMetadataFromServer> =
  combine4EqualityCalls(
    (entry) => entry.title,
    StringKeepDeepEquality,
    (entry) => entry.ownerName,
    NullableStringKeepDeepEquality,
    (entry) => entry.ownerPicture,
    NullableStringKeepDeepEquality,
    (entry) => entry.hasPendingRequests,
    undefinableDeepEquality(BooleanKeepDeepEquality),
    projectMetadataFromServer,
  )

export const ProjectServerStateKeepDeepEquality: KeepDeepEqualityCall<ProjectServerState> =
  combine5EqualityCalls(
    (entry) => entry.isMyProject,
    createCallWithTripleEquals<ProjectServerState['isMyProject']>(),
    (entry) => entry.ownerId,
    NullableStringKeepDeepEquality,
    (entry) => entry.projectData,
    nullableDeepEquality(ProjectMetadataFromServerKeepDeepEquality),
    (entry) => entry.forkedFromProjectData,
    nullableDeepEquality(ProjectMetadataFromServerKeepDeepEquality),
    (entry) => entry.currentlyHolderOfTheBaton,
    createCallWithTripleEquals<boolean>(),
    projectServerState,
  )

export const CollaboratorKeepDeepEquality: KeepDeepEqualityCall<Collaborator> =
  combine3EqualityCalls(
    (data) => data.id,
    StringKeepDeepEquality,
    (data) => data.name,
    undefinableDeepEquality(StringKeepDeepEquality),
    (data) => data.avatar,
    undefinableDeepEquality(StringKeepDeepEquality),
    (id, name, avatar) => ({ id, name, avatar }),
  )

export const MultiplayerSubstateKeepDeepEquality: KeepDeepEqualityCall<MultiplayerSubstate> =
  combine1EqualityCall(
    (entry) => entry.editor.collaborators,
    arrayDeepEquality(CollaboratorKeepDeepEquality),
    (collaborators) => ({ editor: { collaborators: collaborators } }),
  )

export const OnlineStateKeepDeepEquality: KeepDeepEqualityCall<OnlineState> = combine1EqualityCall(
  (status) => status.runningFailureCount,
  createCallWithTripleEquals<number>(),
  onlineState,
)

export function TransientCanvasStateFilesStateKeepDeepEquality(
  oldValue: TransientFilesState,
  newValue: TransientFilesState,
): KeepDeepEqualityResult<TransientFilesState> {
  return getIntrospectiveKeepDeepResult<TransientFilesState>(oldValue, newValue)
}

export const RegularNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<RegularNavigatorEntry> =
  combine1EqualityCall(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    regularNavigatorEntry,
  )

export const ConditionalClauseNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<ConditionalClauseNavigatorEntry> =
  combine2EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.clause,
    createCallWithTripleEquals<ConditionalCase>(),
    conditionalClauseNavigatorEntry,
  )

export const SyntheticNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<SyntheticNavigatorEntry> =
  combine2EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.childOrAttribute,
    JSXElementChildKeepDeepEquality(),
    syntheticNavigatorEntry,
  )

export const RenderedAtKeepDeepEquality: KeepDeepEqualityCall<RenderedAt> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'element-property-path':
      if (oldValue.type === newValue.type) {
        return combine1EqualityCall(
          (r) => r.elementPropertyPath,
          ElementPropertyPathKeepDeepEquality(),
          renderedAtPropertyPath,
        )(oldValue, newValue)
      }
      break
    case 'child-node':
      if (oldValue.type === newValue.type) {
        return combine2EqualityCalls(
          (r) => r.parentPath,
          ElementPathKeepDeepEquality,
          (r) => r.nodeUid,
          StringKeepDeepEquality,
          renderedAtChildNode,
        )(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const DataReferenceNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<DataReferenceNavigatorEntry> =
  combine4EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.renderedAt,
    RenderedAtKeepDeepEquality,
    (entry) => entry.surroundingScope,
    ElementPathKeepDeepEquality,
    (entry) => entry.childOrAttribute,
    JSXElementChildKeepDeepEquality(),
    dataReferenceNavigatorEntry,
  )

export const RenderPropNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<RenderPropNavigatorEntry> =
  combine3EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.propName,
    StringKeepDeepEquality,
    (entry) => entry.childPath,
    nullableDeepEquality(ElementPathKeepDeepEquality),
    renderPropNavigatorEntry,
  )

export const RenderPropValueNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<RenderPropValueNavigatorEntry> =
  combine2EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.prop,
    StringKeepDeepEquality,
    renderPropValueNavigatorEntry,
  )

export const InvalidOverrideNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<InvalidOverrideNavigatorEntry> =
  combine2EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.message,
    StringKeepDeepEquality,
    invalidOverrideNavigatorEntry,
  )

export const SlotNavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<SlotNavigatorEntry> =
  combine2EqualityCalls(
    (entry) => entry.elementPath,
    ElementPathKeepDeepEquality,
    (entry) => entry.prop,
    StringKeepDeepEquality,
    (elementPath, prop) => ({ type: 'SLOT', elementPath: elementPath, prop: prop }),
  )

export const NavigatorRowKeepDeepEquality: KeepDeepEqualityCall<NavigatorRow> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'regular-row':
      if (oldValue.type === newValue.type) {
        return combine2EqualityCalls(
          (row) => row.entry,
          NavigatorEntryKeepDeepEquality,
          (row) => row.indentation,
          createCallWithTripleEquals<number>(),
          regularNavigatorRow,
        )(oldValue, newValue)
      }
      break
    case 'condensed-row':
      if (oldValue.type === newValue.type) {
        return combine3EqualityCalls(
          (row) => row.entries,
          arrayDeepEquality(NavigatorEntryKeepDeepEquality),
          (row) => row.variant,
          createCallWithTripleEquals<'trunk' | 'leaf'>(),
          (row) => row.indentation,
          createCallWithTripleEquals<number>(),
          condensedNavigatorRow,
        )(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const NavigatorEntryKeepDeepEquality: KeepDeepEqualityCall<NavigatorEntry> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'REGULAR':
      if (oldValue.type === newValue.type) {
        return RegularNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'CONDITIONAL_CLAUSE':
      if (oldValue.type === newValue.type) {
        return ConditionalClauseNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'SYNTHETIC':
      if (oldValue.type === newValue.type) {
        return SyntheticNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'DATA_REFERENCE':
      if (oldValue.type === newValue.type) {
        return DataReferenceNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'RENDER_PROP':
      if (oldValue.type === newValue.type) {
        return RenderPropNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'RENDER_PROP_VALUE':
      if (oldValue.type === newValue.type) {
        return RenderPropValueNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'INVALID_OVERRIDE':
      if (oldValue.type === newValue.type) {
        return InvalidOverrideNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'SLOT':
      if (oldValue.type === newValue.type) {
        return SlotNavigatorEntryKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const DropTargetHintKeepDeepEquality: KeepDeepEqualityCall<DropTargetHint> =
  combine4EqualityCalls(
    (hint) => hint.displayAtEntry,
    NavigatorEntryKeepDeepEquality,
    (hint) => hint.targetParent,
    NavigatorEntryKeepDeepEquality,
    (hint) => hint.type,
    createCallWithTripleEquals(),
    (hint) => hint.targetIndexPosition,
    IndexPositionKeepDeepEquality,
    (displayAtElementPath, moveToElementPath, type, targetIndexPosition) => {
      return {
        displayAtEntry: displayAtElementPath,
        targetParent: moveToElementPath,
        type: type,
        targetIndexPosition: targetIndexPosition,
      }
    },
  )

export const NavigatorStateKeepDeepEquality: KeepDeepEqualityCall<NavigatorState> =
  combine6EqualityCalls(
    (state) => state.minimised,
    createCallWithTripleEquals(),
    (state) => state.dropTargetHint,
    nullableDeepEquality(DropTargetHintKeepDeepEquality),
    (state) => state.collapsedViews,
    ElementPathArrayKeepDeepEquality,
    (state) => state.renamingTarget,
    nullableDeepEquality(ElementPathKeepDeepEquality),
    (state) => state.highlightedTargets,
    ElementPathArrayKeepDeepEquality,
    (state) => state.hiddenInNavigator,
    ElementPathArrayKeepDeepEquality,
    (
      minimised,
      dropTargetHint,
      collapsedViews,
      renamingTarget,
      highlightedTargets,
      hiddenInNavigator,
    ) => {
      return {
        minimised: minimised,
        dropTargetHint: dropTargetHint,
        collapsedViews: collapsedViews,
        renamingTarget: renamingTarget,
        highlightedTargets: highlightedTargets,
        hiddenInNavigator: hiddenInNavigator,
      }
    },
  )

export function DerivedStateKeepDeepEquality(): KeepDeepEqualityCall<DerivedState> {
  return combine8EqualityCalls(
    (state) => state.visibleNavigatorTargets,
    arrayDeepEquality(NavigatorEntryKeepDeepEquality),
    (state) => state.autoFocusedPaths,
    arrayDeepEquality(ElementPathKeepDeepEquality),
    (state) => state.controls,
    HigherOrderControlArrayKeepDeepEquality,
    (state) => state.elementWarnings,
    objectDeepEquality(ElementWarningsKeepDeepEquality),
    (state) => state.projectContentsChecksums,
    FileChecksumsWithFileKeepDeepEquality,
    (state) => state.branchOriginContentsChecksums,
    nullableDeepEquality(FileChecksumsWithFileKeepDeepEquality),
    (state) => state.remixData,
    createCallWithTripleEquals(),
    (state) => state.filePathMappings,
    createCallWithShallowEquals(),
    (
      visibleNavigatorTargets,
      autoFocusedPaths,
      controls,
      elementWarnings,
      projectContentsChecksums,
      branchOriginContentsChecksums,
      remixData,
      filePathMappings,
    ) => {
      return {
        visibleNavigatorTargets: visibleNavigatorTargets,
        autoFocusedPaths: autoFocusedPaths,
        controls: controls,
        elementWarnings: elementWarnings,
        projectContentsChecksums: projectContentsChecksums,
        branchOriginContentsChecksums: branchOriginContentsChecksums,
        remixData: remixData,
        filePathMappings: filePathMappings,
      }
    },
  )
}

export const MultiLineCommentKeepDeepEqualityCall: KeepDeepEqualityCall<MultiLineComment> =
  combine4EqualityCalls(
    (comment) => comment.comment,
    StringKeepDeepEquality,
    (comment) => comment.rawText,
    StringKeepDeepEquality,
    (comment) => comment.trailingNewLine,
    BooleanKeepDeepEquality,
    (comment) => comment.pos,
    NullableNumberKeepDeepEquality,
    multiLineComment,
  )

export const SingleLineCommentKeepDeepEqualityCall: KeepDeepEqualityCall<SingleLineComment> =
  combine4EqualityCalls(
    (comment) => comment.comment,
    StringKeepDeepEquality,
    (comment) => comment.rawText,
    StringKeepDeepEquality,
    (comment) => comment.trailingNewLine,
    BooleanKeepDeepEquality,
    (comment) => comment.pos,
    NullableNumberKeepDeepEquality,
    singleLineComment,
  )

export const CommentKeepDeepEqualityCall: KeepDeepEqualityCall<Comment> = (oldValue, newValue) => {
  if (isMultiLineComment(oldValue) && isMultiLineComment(newValue)) {
    return MultiLineCommentKeepDeepEqualityCall(oldValue, newValue)
  } else if (isSingleLineComment(oldValue) && isSingleLineComment(newValue)) {
    return SingleLineCommentKeepDeepEqualityCall(oldValue, newValue)
  } else {
    return keepDeepEqualityResult(newValue, false)
  }
}

export const ParsedCommentsKeepDeepEqualityCall: KeepDeepEqualityCall<ParsedComments> =
  combine2EqualityCalls(
    (comments) => comments.leadingComments,
    arrayDeepEquality(CommentKeepDeepEqualityCall),
    (comments) => comments.trailingComments,
    arrayDeepEquality(CommentKeepDeepEqualityCall),
    parsedComments,
  )

export function JSXAttributeValueValueKeepDeepEqualityCall(
  oldValue: unknown,
  newValue: unknown,
): KeepDeepEqualityResult<unknown> {
  return getIntrospectiveKeepDeepResult<unknown>(oldValue, newValue)
}

export const JSXAttributeValueKeepDeepEqualityCall: KeepDeepEqualityCall<JSExpressionValue<any>> =
  combine3EqualityCalls(
    (attribute) => attribute.value,
    JSXAttributeValueValueKeepDeepEqualityCall,
    (attribute) => attribute.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (attribute) => attribute.uid,
    createCallWithTripleEquals<string>(),
    jsExpressionValue,
  )

export const RawSourceMapKeepDeepEquality: KeepDeepEqualityCall<RawSourceMap> =
  combine8EqualityCalls(
    (map) => map.version,
    NumberKeepDeepEquality,
    (map) => map.sources,
    arrayDeepEquality(StringKeepDeepEquality),
    (map) => map.names,
    arrayDeepEquality(StringKeepDeepEquality),
    (map) => map.sourceRoot,
    undefinableDeepEquality(StringKeepDeepEquality),
    (map) => map.sourcesContent,
    undefinableDeepEquality(arrayDeepEquality(StringKeepDeepEquality)),
    (map) => map.transpiledContentUtopia,
    undefinableDeepEquality(StringKeepDeepEquality),
    (map) => map.mappings,
    StringKeepDeepEquality,
    (map) => map.file,
    StringKeepDeepEquality,
    (
      version,
      sources,
      names,
      sourceRoot,
      sourcesContent,
      transpiledContentUtopia,
      mappings,
      file,
    ) => {
      return {
        version: version,
        sources: sources,
        names: names,
        sourceRoot: sourceRoot,
        sourcesContent: sourcesContent,
        transpiledContentUtopia: transpiledContentUtopia,
        mappings: mappings,
        file: file,
      }
    },
  )

export function JSAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<JSAssignment> {
  return combine2EqualityCalls(
    (assignment) => assignment.leftHandSide,
    BoundParamKeepDeepEquality(),
    (assignment) => assignment.rightHandSide,
    JSExpressionKeepDeepEqualityCall,
    jsAssignment,
  )
}

export function JSAssignmentStatementKeepDeepEqualityCall(): KeepDeepEqualityCall<JSAssignmentStatement> {
  return combine3EqualityCalls(
    (statement) => statement.declarationKeyword,
    createCallWithTripleEquals<'let' | 'const' | 'var'>(),
    (statement) => statement.assignments,
    arrayDeepEquality(JSAssignmentKeepDeepEqualityCall()),
    (statement) => statement.uid,
    createCallWithTripleEquals<string>(),
    jsAssignmentStatement,
  )
}

export const JSOpaqueArbitraryStatementKeepDeepEqualityCall: KeepDeepEqualityCall<JSOpaqueArbitraryStatement> =
  combine4EqualityCalls(
    (statement) => statement.originalJavascript,
    createCallWithTripleEquals<string>(),
    (statement) => statement.definedWithin,
    arrayDeepEquality(createCallWithTripleEquals<string>()),
    (statement) => statement.definedElsewhere,
    arrayDeepEquality(createCallWithTripleEquals<string>()),
    (statement) => statement.uid,
    createCallWithTripleEquals<string>(),
    jsOpaqueArbitraryStatement,
  )

export const JSArbitraryStatementKeepDeepEqualityCall: KeepDeepEqualityCall<
  JSArbitraryStatement
> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'JS_OPAQUE_ARBITRARY_STATEMENT':
      if (oldValue.type === newValue.type) {
        return JSOpaqueArbitraryStatementKeepDeepEqualityCall(oldValue, newValue)
      }
      break
    case 'JS_ASSIGNMENT_STATEMENT':
      if (oldValue.type === newValue.type) {
        return JSAssignmentStatementKeepDeepEqualityCall()(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export function JSExpressionOtherJavaScriptKeepDeepEqualityCall(): KeepDeepEqualityCall<JSExpressionOtherJavaScript> {
  return combine9EqualityCalls(
    (attribute) => attribute.params,
    arrayDeepEquality(ParamKeepDeepEquality()),
    (attribute) => attribute.originalJavascript,
    createCallWithTripleEquals<string>(),
    (attribute) => attribute.javascriptWithUIDs,
    createCallWithTripleEquals<string>(),
    (attribute) => attribute.transpiledJavascript,
    createCallWithTripleEquals<string>(),
    (attribute) => attribute.definedElsewhere,
    arrayDeepEquality(createCallWithTripleEquals()),
    (attribute) => attribute.sourceMap,
    nullableDeepEquality(RawSourceMapKeepDeepEquality),
    (block) => block.elementsWithin,
    ElementsWithinKeepDeepEqualityCall(),
    (block) => block.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (attribute) => attribute.uid,
    createCallWithTripleEquals(),
    jsExpressionOtherJavaScript,
  )
}

export function JSXMapExpressionKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXMapExpression> {
  return combine5EqualityCalls(
    (expression) => expression.valueToMap,
    JSExpressionKeepDeepEqualityCall,
    (expression) => expression.mapFunction,
    JSExpressionKeepDeepEqualityCall,
    (expression) => expression.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (block) => block.valuesInScopeFromParameters,
    arrayDeepEquality(createCallWithTripleEquals<string>()),
    (expression) => expression.uid,
    createCallWithTripleEquals<string>(),
    jsxMapExpression,
  )
}

export const JSExpressionOtherJavaScriptOrJSXMapExpressionKeepDeepEqualityCall: KeepDeepEqualityCall<
  JSExpressionOtherJavaScript | JSXMapExpression
> = (oldValue, newValue) => {
  if (isJSExpressionOtherJavaScript(oldValue) && isJSExpressionOtherJavaScript(newValue)) {
    return JSExpressionOtherJavaScriptKeepDeepEqualityCall()(oldValue, newValue)
  } else if (isJSXMapExpression(oldValue) && isJSXMapExpression(newValue)) {
    return JSXMapExpressionKeepDeepEqualityCall()(oldValue, newValue)
  } else {
    return keepDeepEqualityResult(newValue, false)
  }
}

export function JSXArrayValueKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArrayValue> {
  return combine2EqualityCalls(
    (value) => value.value,
    JSExpressionKeepDeepEqualityCall,
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall,
    jsxArrayValue,
  )
}

export function JSXArraySpreadKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArraySpread> {
  return combine2EqualityCalls(
    (value) => value.value,
    JSExpressionKeepDeepEqualityCall,
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall,
    jsxArraySpread,
  )
}

export function JSXArrayElementKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXArrayElement> {
  return (oldElement, newElement) => {
    if (isArrayValue(oldElement) && isArrayValue(newElement)) {
      return JSXArrayValueKeepDeepEqualityCall()(oldElement, newElement)
    } else if (isArraySpread(oldElement) && isArraySpread(newElement)) {
      return JSXArraySpreadKeepDeepEqualityCall()(oldElement, newElement)
    } else {
      return keepDeepEqualityResult(newElement, false)
    }
  }
}

export function JSXAttributeNestedArrayKeepDeepEqualityCall(): KeepDeepEqualityCall<JSExpressionNestedArray> {
  return combine3EqualityCalls(
    (attribute) => attribute.content,
    arrayDeepEquality(JSXArrayElementKeepDeepEqualityCall()),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (attribute) => attribute.uid,
    createCallWithTripleEquals<string>(),
    jsExpressionNestedArray,
  )
}

export function JSXSpreadAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXSpreadAssignment> {
  return combine2EqualityCalls(
    (value) => value.value,
    JSExpressionKeepDeepEqualityCall,
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall,
    jsxSpreadAssignment,
  )
}

export function JSXPropertyAssignmentKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXPropertyAssignment> {
  return combine4EqualityCalls(
    (value) => value.key,
    createCallWithTripleEquals(),
    (value) => value.value,
    JSExpressionKeepDeepEqualityCall,
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (value) => value.keyComments,
    ParsedCommentsKeepDeepEqualityCall,
    jsxPropertyAssignment,
  )
}

export function JSXPropertyKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXProperty> {
  return (oldProperty, newProperty) => {
    if (isSpreadAssignment(oldProperty) && isSpreadAssignment(newProperty)) {
      return JSXSpreadAssignmentKeepDeepEqualityCall()(oldProperty, newProperty)
    } else if (isPropertyAssignment(oldProperty) && isPropertyAssignment(newProperty)) {
      return JSXPropertyAssignmentKeepDeepEqualityCall()(oldProperty, newProperty)
    } else {
      return keepDeepEqualityResult(newProperty, false)
    }
  }
}

export function JSXAttributeNestedObjectKeepDeepEqualityCall(): KeepDeepEqualityCall<JSExpressionNestedObject> {
  return combine3EqualityCalls(
    (attribute) => attribute.content,
    arrayDeepEquality(JSXPropertyKeepDeepEqualityCall()),
    (value) => value.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (value) => value.uid,
    createCallWithTripleEquals<string>(),
    jsExpressionNestedObject,
  )
}

export function JSXAttributeFunctionCallKeepDeepEqualityCall(): KeepDeepEqualityCall<JSExpressionFunctionCall> {
  return combine3EqualityCalls(
    (value) => value.functionName,
    createCallWithTripleEquals(),
    (value) => value.parameters,
    arrayDeepEquality(JSExpressionKeepDeepEqualityCall),
    (value) => value.uid,
    createCallWithTripleEquals<string>(),
    jsExpressionFunctionCall,
  )
}

export const JSExpressionKeepDeepEqualityCall: KeepDeepEqualityCall<JSExpression> = (
  oldAttribute,
  newAttribute,
) => {
  switch (oldAttribute.type) {
    case 'JSX_MAP_EXPRESSION':
      if (isJSXMapExpression(newAttribute)) {
        return JSXMapExpressionKeepDeepEqualityCall()(oldAttribute, newAttribute)
      }
      break
    case 'ATTRIBUTE_VALUE':
      if (isJSExpressionValue(newAttribute)) {
        return JSXAttributeValueKeepDeepEqualityCall(oldAttribute, newAttribute)
      }
      break
    case 'JS_IDENTIFIER':
      if (isJSIdentifier(newAttribute)) {
        return JSIdentifierKeepDeepEquality()(oldAttribute, newAttribute)
      }
      break
    case 'JS_ELEMENT_ACCESS':
      if (isJSElementAccess(newAttribute)) {
        return JSElementAccessKeepDeepEquality()(oldAttribute, newAttribute)
      }
      break
    case 'JS_PROPERTY_ACCESS':
      if (isJSPropertyAccess(newAttribute)) {
        return JSPropertyAccessKeepDeepEquality()(oldAttribute, newAttribute)
      }
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      if (isJSExpressionNestedArray(newAttribute)) {
        return JSXAttributeNestedArrayKeepDeepEqualityCall()(oldAttribute, newAttribute)
      }
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      if (isJSExpressionNestedObject(newAttribute)) {
        return JSXAttributeNestedObjectKeepDeepEqualityCall()(oldAttribute, newAttribute)
      }
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      if (isJSExpressionFunctionCall(newAttribute)) {
        return JSXAttributeFunctionCallKeepDeepEqualityCall()(oldAttribute, newAttribute)
      }
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      if (isJSExpressionOtherJavaScript(newAttribute)) {
        return JSExpressionOtherJavaScriptKeepDeepEqualityCall()(oldAttribute, newAttribute)
      }
      break
    case 'JSX_ELEMENT':
      if (isJSXElement(newAttribute)) {
        return JSXElementKeepDeepEquality(oldAttribute, newAttribute)
      }
      break
    default:
      assertNever(oldAttribute)
  }

  return keepDeepEqualityResult(newAttribute, false)
}

export function JSXAttributesEntryDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributesEntry> {
  return combine3EqualityCalls(
    (entry) => entry.key,
    createCallWithTripleEquals(),
    (entry) => entry.value,
    JSExpressionKeepDeepEqualityCall,
    (entry) => entry.comments,
    ParsedCommentsKeepDeepEqualityCall,
    jsxAttributesEntry,
  )
}

export function JSXAttributesSpreadDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributesSpread> {
  return combine2EqualityCalls(
    (entry) => entry.spreadValue,
    JSExpressionKeepDeepEqualityCall,
    (entry) => entry.comments,
    ParsedCommentsKeepDeepEqualityCall,
    jsxAttributesSpread,
  )
}

export function JSXAttributesPartDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributesPart> {
  return (oldPart, newPart) => {
    if (isJSXAttributesEntry(oldPart) && isJSXAttributesEntry(newPart)) {
      return JSXAttributesEntryDeepEqualityCall()(oldPart, newPart)
    } else if (isJSXAttributesSpread(oldPart) && isJSXAttributesSpread(newPart)) {
      return JSXAttributesSpreadDeepEqualityCall()(oldPart, newPart)
    } else {
      return keepDeepEqualityResult(newPart, false)
    }
  }
}

export function JSXAttributesKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXAttributes> {
  return arrayDeepEquality(JSXAttributesPartDeepEqualityCall())
}

export const JSXElementChildArrayKeepDeepEquality: KeepDeepEqualityCall<Array<JSXElementChild>> =
  arrayDeepEquality(JSXElementChildKeepDeepEquality())

export const JSXElementKeepDeepEquality: KeepDeepEqualityCall<JSXElement> = combine4EqualityCalls(
  (element) => element.name,
  JSXElementNameKeepDeepEqualityCall,
  (element) => element.uid,
  StringKeepDeepEquality,
  (element) => element.props,
  JSXAttributesKeepDeepEqualityCall(),
  (element) => element.children,
  JSXElementChildArrayKeepDeepEquality,
  jsxElement,
)

export function JSXElementWithoutUIDKeepDeepEquality(): KeepDeepEqualityCall<JSXElementWithoutUID> {
  return combine3EqualityCalls(
    (element) => element.name,
    JSXElementNameKeepDeepEqualityCall,
    (element) => element.props,
    JSXAttributesKeepDeepEqualityCall(),
    (element) => element.children,
    JSXElementChildArrayKeepDeepEquality,
    jsxElementWithoutUID,
  )
}

export function ElementsWithinKeepDeepEqualityCall(): KeepDeepEqualityCall<ElementsWithin> {
  return objectDeepEquality(JSXElementKeepDeepEquality)
}

export function ArbitraryJSBlockKeepDeepEquality(): KeepDeepEqualityCall<ArbitraryJSBlock> {
  return combine9EqualityCalls(
    (block) => block.params,
    arrayDeepEquality(ParamKeepDeepEquality()),
    (block) => block.javascript,
    createCallWithTripleEquals(),
    (block) => block.transpiledJavascript,
    createCallWithTripleEquals(),
    (block) => block.definedWithin,
    arrayDeepEquality(createCallWithTripleEquals()),
    (block) => block.definedElsewhere,
    arrayDeepEquality(createCallWithTripleEquals()),
    (block) => block.sourceMap,
    nullableDeepEquality(RawSourceMapKeepDeepEquality),
    (block) => block.elementsWithin,
    ElementsWithinKeepDeepEqualityCall(),
    (attribute) => attribute.statements,
    arrayDeepEquality(JSArbitraryStatementKeepDeepEqualityCall),
    (block) => block.uid,
    createCallWithTripleEquals(),
    arbitraryJSBlock,
  )
}

export function JSIdentifierKeepDeepEquality(): KeepDeepEqualityCall<JSIdentifier> {
  return combine4EqualityCalls(
    (identifier) => identifier.name,
    createCallWithTripleEquals<string>(),
    (identifier) => identifier.uid,
    createCallWithTripleEquals<string>(),
    (identifier) => identifier.sourceMap,
    nullableDeepEquality(RawSourceMapKeepDeepEquality),
    (identifier) => identifier.comments,
    ParsedCommentsKeepDeepEqualityCall,
    jsIdentifier,
  )
}

export function JSPropertyAccessKeepDeepEquality(): KeepDeepEqualityCall<JSPropertyAccess> {
  return combine7EqualityCalls(
    (access) => access.onValue,
    JSExpressionKeepDeepEqualityCall,
    (access) => access.property,
    createCallWithTripleEquals<string>(),
    (access) => access.uid,
    createCallWithTripleEquals<string>(),
    (access) => access.sourceMap,
    nullableDeepEquality(RawSourceMapKeepDeepEquality),
    (access) => access.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (access) => access.originalJavascript,
    createCallWithTripleEquals<string>(),
    (access) => access.optionallyChained,
    createCallWithTripleEquals<OptionallyChained>(),
    jsPropertyAccess,
  )
}

export function JSElementAccessKeepDeepEquality(): KeepDeepEqualityCall<JSElementAccess> {
  return combine7EqualityCalls(
    (access) => access.onValue,
    JSExpressionKeepDeepEqualityCall,
    (access) => access.element,
    JSExpressionKeepDeepEqualityCall,
    (access) => access.uid,
    createCallWithTripleEquals<string>(),
    (access) => access.sourceMap,
    nullableDeepEquality(RawSourceMapKeepDeepEquality),
    (access) => access.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (access) => access.originalJavascript,
    createCallWithTripleEquals<string>(),
    (access) => access.optionallyChained,
    createCallWithTripleEquals<OptionallyChained>(),
    jsElementAccess,
  )
}

export function JSXElementChildKeepDeepEquality(): KeepDeepEqualityCall<JSXElementChild> {
  return (oldElement, newElement) => {
    switch (oldElement.type) {
      case 'JSX_ELEMENT':
        if (isJSXElement(newElement)) {
          return JSXElementKeepDeepEquality(oldElement, newElement)
        }
        break
      case 'JSX_FRAGMENT':
        if (isJSXFragment(newElement)) {
          return JSXFragmentKeepDeepEquality(oldElement, newElement)
        }
        break
      case 'JSX_TEXT_BLOCK':
        if (isJSXTextBlock(newElement)) {
          return JSXTextBlockKeepDeepEquality(oldElement, newElement)
        }
        break
      case 'JSX_CONDITIONAL_EXPRESSION':
        if (isJSXConditionalExpression(newElement)) {
          return JSXConditionalExpressionKeepDeepEquality(oldElement, newElement)
        }
        break
      case 'JSX_MAP_EXPRESSION':
        if (isJSXMapExpression(newElement)) {
          return JSXMapExpressionKeepDeepEqualityCall()(oldElement, newElement)
        }
        break
      case 'ATTRIBUTE_VALUE':
        if (isJSExpressionValue(newElement)) {
          return JSXAttributeValueKeepDeepEqualityCall(oldElement, newElement)
        }
        break
      case 'JS_IDENTIFIER':
        if (isJSIdentifier(newElement)) {
          return JSIdentifierKeepDeepEquality()(oldElement, newElement)
        }
        break
      case 'JS_ELEMENT_ACCESS':
        if (isJSElementAccess(newElement)) {
          return JSElementAccessKeepDeepEquality()(oldElement, newElement)
        }
        break
      case 'JS_PROPERTY_ACCESS':
        if (isJSPropertyAccess(newElement)) {
          return JSPropertyAccessKeepDeepEquality()(oldElement, newElement)
        }
        break
      case 'ATTRIBUTE_NESTED_ARRAY':
        if (isJSExpressionNestedArray(newElement)) {
          return JSXAttributeNestedArrayKeepDeepEqualityCall()(oldElement, newElement)
        }
        break
      case 'ATTRIBUTE_NESTED_OBJECT':
        if (isJSExpressionNestedObject(newElement)) {
          return JSXAttributeNestedObjectKeepDeepEqualityCall()(oldElement, newElement)
        }
        break
      case 'ATTRIBUTE_FUNCTION_CALL':
        if (isJSExpressionFunctionCall(newElement)) {
          return JSXAttributeFunctionCallKeepDeepEqualityCall()(oldElement, newElement)
        }
        break
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        if (isJSExpressionOtherJavaScript(newElement)) {
          return JSExpressionOtherJavaScriptKeepDeepEqualityCall()(oldElement, newElement)
        }
        break
      default:
        assertNever(oldElement)
    }

    return keepDeepEqualityResult(newElement, false)
  }
}

export const SimpleFunctionWrapKeepDeepEquality: KeepDeepEqualityCall<SimpleFunctionWrap> =
  combine1EqualityCall(
    (wrap) => wrap.functionExpression,
    JSExpressionKeepDeepEqualityCall,
    simpleFunctionWrap,
  )

export const FunctionWrapKeepDeepEquality: KeepDeepEqualityCall<FunctionWrap> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'SIMPLE_FUNCTION_WRAP':
      if (isSimpleFunctionWrap(newValue)) {
        return SimpleFunctionWrapKeepDeepEquality(oldValue, newValue)
      }
      break
  }
  return keepDeepEqualityResult(newValue, false)
}

export const UtopiaJSXComponentKeepDeepEquality: KeepDeepEqualityCall<UtopiaJSXComponent> =
  combine11EqualityCalls(
    (component) => component.name,
    createCallWithTripleEquals(),
    (component) => component.isFunction,
    createCallWithTripleEquals(),
    (component) => component.declarationSyntax,
    createCallWithTripleEquals(),
    (component) => component.blockOrExpression,
    createCallWithTripleEquals(),
    (component) => component.functionWrapping,
    arrayDeepEquality(FunctionWrapKeepDeepEquality),
    (component) => component.params,
    nullableDeepEquality(arrayDeepEquality(ParamKeepDeepEquality())),
    (component) => component.propsUsed,
    arrayDeepEquality(createCallWithTripleEquals()),
    (component) => component.rootElement,
    JSXElementChildKeepDeepEquality(),
    (component) => component.arbitraryJSBlock,
    nullableDeepEquality(ArbitraryJSBlockKeepDeepEquality()),
    (component) => component.usedInReactDOMRender,
    createCallWithTripleEquals(),
    (component) => component.returnStatementComments,
    ParsedCommentsKeepDeepEqualityCall,
    utopiaJSXComponent,
  )

export const ImportStatementKeepDeepEquality: KeepDeepEqualityCall<ImportStatement> =
  combine5EqualityCalls(
    (statement) => statement.rawCode,
    StringKeepDeepEquality,
    (statement) => statement.importStarAs,
    BooleanKeepDeepEquality,
    (statement) => statement.importWithName,
    BooleanKeepDeepEquality,
    (statement) => statement.imports,
    arrayDeepEquality(StringKeepDeepEquality),
    (statement) => statement.module,
    StringKeepDeepEquality,
    importStatement,
  )

export const UnparsedCodeKeepDeepEquality: KeepDeepEqualityCall<UnparsedCode> =
  combine1EqualityCall((unparsed) => unparsed.rawCode, StringKeepDeepEquality, unparsedCode)

export const TopLevelElementKeepDeepEquality: KeepDeepEqualityCall<TopLevelElement> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'UTOPIA_JSX_COMPONENT':
      if (newValue.type === oldValue.type) {
        return UtopiaJSXComponentKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'ARBITRARY_JS_BLOCK':
      if (newValue.type === oldValue.type) {
        return ArbitraryJSBlockKeepDeepEquality()(oldValue, newValue)
      }
      break
    case 'IMPORT_STATEMENT':
      if (newValue.type === oldValue.type) {
        return ImportStatementKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'UNPARSED_CODE':
      if (newValue.type === oldValue.type) {
        return UnparsedCodeKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const JSXTextBlockKeepDeepEquality: KeepDeepEqualityCall<JSXTextBlock> =
  combine2EqualityCalls(
    (block) => block.text,
    createCallWithTripleEquals(),
    (block) => block.uid,
    createCallWithTripleEquals(),
    (text, uniqueID) => {
      return {
        type: 'JSX_TEXT_BLOCK',
        text: text,
        uid: uniqueID,
      }
    },
  )

export const JSXFragmentKeepDeepEquality: KeepDeepEqualityCall<JSXFragment> = combine3EqualityCalls(
  (fragment) => fragment.children,
  JSXElementChildArrayKeepDeepEquality,
  (fragment) => fragment.uid,
  StringKeepDeepEquality,
  (fragment) => fragment.longForm,
  BooleanKeepDeepEquality,
  (children, uid, longForm) => {
    return {
      type: 'JSX_FRAGMENT',
      children: children,
      uid: uid,
      longForm: longForm,
    }
  },
)

export const JSXConditionalExpressionKeepDeepEquality: KeepDeepEqualityCall<JSXConditionalExpression> =
  combine6EqualityCalls(
    (conditional) => conditional.uid,
    StringKeepDeepEquality,
    (conditional) => conditional.condition,
    JSExpressionKeepDeepEqualityCall,
    (conditional) => conditional.originalConditionString,
    StringKeepDeepEquality,
    (conditional) => conditional.whenTrue,
    JSXElementChildKeepDeepEquality(),
    (conditional) => conditional.whenFalse,
    JSXElementChildKeepDeepEquality(),
    (conditional) => conditional.comments,
    ParsedCommentsKeepDeepEqualityCall,
    (
      uid,
      condition,
      originalConditionString,
      whenTrue,
      whenFalse,
      comments,
    ): JSXConditionalExpression => {
      return jsxConditionalExpression(
        uid,
        condition,
        originalConditionString,
        whenTrue,
        whenFalse,
        comments,
      )
    },
  )

export const RegularParamKeepDeepEquality: KeepDeepEqualityCall<RegularParam> =
  combine2EqualityCalls(
    (param) => param.paramName,
    createCallWithTripleEquals(),
    (param) => param.defaultExpression,
    nullableDeepEquality(JSExpressionKeepDeepEqualityCall),
    regularParam,
  )

export const DestructuredParamPartKeepDeepEquality: KeepDeepEqualityCall<DestructuredParamPart> =
  combine3EqualityCalls(
    (paramPart) => paramPart.propertyName,
    createCallWithTripleEquals(),
    (paramPart) => paramPart.param,
    ParamKeepDeepEquality(),
    (paramPart) => paramPart.defaultExpression,
    nullableDeepEquality(JSExpressionKeepDeepEqualityCall),
    destructuredParamPart,
  )

export const DestructuredObjectParamKeepDeepEquality: KeepDeepEqualityCall<DestructuredObject> =
  combine1EqualityCall(
    (paramPart) => paramPart.parts,
    arrayDeepEquality(DestructuredParamPartKeepDeepEquality),
    destructuredObject,
  )

export const DestructuredArrayPartKeepDeepEquality: KeepDeepEqualityCall<DestructuredArrayPart> = (
  oldValue,
  newValue,
) => {
  if (isOmittedParam(oldValue) && isOmittedParam(newValue)) {
    return keepDeepEqualityResult(oldValue, true)
  } else if (isParam(oldValue) && isParam(newValue)) {
    return ParamKeepDeepEquality()(oldValue, newValue)
  } else {
    return keepDeepEqualityResult(newValue, false)
  }
}

export const DestructuredArrayKeepDeepEquality: KeepDeepEqualityCall<DestructuredArray> =
  combine1EqualityCall(
    (param) => param.parts,
    arrayDeepEquality(DestructuredArrayPartKeepDeepEquality),
    destructuredArray,
  )

export function BoundParamKeepDeepEquality(): KeepDeepEqualityCall<BoundParam> {
  return (oldValue, newValue) => {
    if (isRegularParam(oldValue) && isRegularParam(newValue)) {
      return RegularParamKeepDeepEquality(oldValue, newValue)
    } else if (isDestructuredObject(oldValue) && isDestructuredObject(newValue)) {
      return DestructuredObjectParamKeepDeepEquality(oldValue, newValue)
    } else if (isDestructuredArray(oldValue) && isDestructuredArray(newValue)) {
      return DestructuredArrayKeepDeepEquality(oldValue, newValue)
    } else {
      return keepDeepEqualityResult(newValue, false)
    }
  }
}

export function ParamKeepDeepEquality(): KeepDeepEqualityCall<Param> {
  return combine2EqualityCalls(
    (param) => param.dotDotDotToken,
    createCallWithTripleEquals(),
    (param) => param.boundParam,
    BoundParamKeepDeepEquality(),
    functionParam,
  )
}

function RectangleKeepDeepEquality<C extends CoordinateMarker>(
  oldValue: Rectangle<C>,
  newValue: Rectangle<C>,
): KeepDeepEqualityResult<Rectangle<C>> {
  if (
    oldValue.x === newValue.x &&
    oldValue.y === newValue.y &&
    oldValue.width === newValue.width &&
    oldValue.height === newValue.height
  ) {
    return keepDeepEqualityResult(oldValue, true)
  } else {
    return keepDeepEqualityResult(newValue, false)
  }
}

function MaybeInfinityRectangleKeepDeepEquality<C extends CoordinateMarker>(
  oldValue: MaybeInfinityRectangle<C>,
  newValue: MaybeInfinityRectangle<C>,
): KeepDeepEqualityResult<MaybeInfinityRectangle<C>> {
  if (isInfinityRectangle(oldValue) || isInfinityRectangle(newValue)) {
    return keepDeepEqualityResult(newValue, oldValue === newValue)
  } else {
    return RectangleKeepDeepEquality(oldValue, newValue)
  }
}

export const CanvasRectangleKeepDeepEquality: KeepDeepEqualityCall<CanvasRectangle> =
  RectangleKeepDeepEquality

export const MaybeInfinityCanvasRectangleKeepDeepEquality: KeepDeepEqualityCall<MaybeInfinityCanvasRectangle> =
  MaybeInfinityRectangleKeepDeepEquality

export function FrameAndTargetKeepDeepEqualityCall<
  C extends CoordinateMarker,
>(): KeepDeepEqualityCall<FrameAndTarget<C>> {
  return combine2EqualityCalls(
    (frameAndTarget) => frameAndTarget.frame,
    RectangleKeepDeepEquality,
    (frameAndTarget) => frameAndTarget.target,
    ElementPathKeepDeepEquality,
    (frame, target) => {
      return {
        frame: frame,
        target: target,
      }
    },
  )
}

export const CanvasFrameAndTargetKeepDeepEquality: KeepDeepEqualityCall<CanvasFrameAndTarget> =
  FrameAndTargetKeepDeepEqualityCall<CanvasRectangle>()

export const LocalRectangleKeepDeepEquality: KeepDeepEqualityCall<LocalRectangle> =
  RectangleKeepDeepEquality

export const MaybeInfinityLocalRectangleKeepDeepEquality: KeepDeepEqualityCall<MaybeInfinityLocalRectangle> =
  MaybeInfinityRectangleKeepDeepEquality

export function LocalPointKeepDeepEquality(
  oldPoint: LocalPoint,
  newPoint: LocalPoint,
): KeepDeepEqualityResult<LocalPoint> {
  if (oldPoint.x === newPoint.x && oldPoint.y === newPoint.y) {
    return keepDeepEqualityResult(oldPoint, true)
  } else {
    return keepDeepEqualityResult(newPoint, false)
  }
}

export function SidesKeepDeepEquality(
  oldSides: Sides,
  newSides: Sides,
): KeepDeepEqualityResult<Sides> {
  if (
    oldSides.top === newSides.top &&
    oldSides.left === newSides.left &&
    oldSides.right === newSides.right &&
    oldSides.bottom === newSides.bottom
  ) {
    return keepDeepEqualityResult(oldSides, true)
  } else {
    return keepDeepEqualityResult(newSides, false)
  }
}

const SameFileOriginKeepDeepEquality: KeepDeepEqualityCall<SameFileOrigin> = combine2EqualityCalls(
  (i) => i.filePath,
  createCallWithTripleEquals(),
  (i) => i.variableName,
  createCallWithTripleEquals(),
  sameFileOrigin,
)

const ImportedOriginKeepDeepEquality: KeepDeepEqualityCall<ImportedOrigin> = combine3EqualityCalls(
  (i) => i.filePath,
  createCallWithTripleEquals(),
  (i) => i.variableName,
  createCallWithTripleEquals(),
  (i) => i.exportedName,
  createCallWithTripleEquals(),
  importedOrigin,
)

export const ImportInfoKeepDeepEquality: KeepDeepEqualityCall<ImportInfo> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'SAME_FILE_ORIGIN':
      if (newValue.type === oldValue.type) {
        return SameFileOriginKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'IMPORTED_ORIGIN':
      if (newValue.type === oldValue.type) {
        return ImportedOriginKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const CSSNumberKeepDeepEquality: KeepDeepEqualityCall<CSSNumber> = combine2EqualityCalls(
  (cssNum) => cssNum.value,
  createCallWithTripleEquals<number>(),
  (cssNum) => cssNum.unit,
  undefinableDeepEquality(nullableDeepEquality(createCallWithTripleEquals<CSSNumberUnit>())),
  cssNumber,
)

export const GridCSSNumberKeepDeepEquality: KeepDeepEqualityCall<GridCSSNumber> =
  combine3EqualityCalls(
    (cssNum) => cssNum.value,
    createCallWithTripleEquals<number>(),
    (cssNum) => cssNum.unit,
    nullableDeepEquality(createCallWithTripleEquals<GridCSSNumberUnit>()),
    (cssNum) => cssNum.areaName,
    nullableDeepEquality(StringKeepDeepEquality),
    gridCSSNumber,
  )

export const GridAutoOrTemplateDimensionsKeepDeepEquality: KeepDeepEqualityCall<GridAutoOrTemplateDimensions> =
  combine1EqualityCall(
    (value) => value.dimensions,
    arrayDeepEquality(GridCSSNumberKeepDeepEquality),
    gridAutoOrTemplateDimensions,
  )

export const GridAutoOrTemplateFallbackKeepDeepEquality: KeepDeepEqualityCall<GridAutoOrTemplateFallback> =
  combine1EqualityCall(
    (value) => value.value,
    createCallWithTripleEquals<string>(),
    gridAutoOrTemplateFallback,
  )

export const GridAutoOrTemplateBaseKeepDeepEquality: KeepDeepEqualityCall<
  GridAutoOrTemplateBase
> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'DIMENSIONS':
      if (newValue.type === oldValue.type) {
        return GridAutoOrTemplateDimensionsKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'FALLBACK':
      if (newValue.type === oldValue.type) {
        return GridAutoOrTemplateFallbackKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const GridTemplateKeepDeepEquality: KeepDeepEqualityCall<GridTemplate> =
  GridAutoOrTemplateBaseKeepDeepEquality

export const GridAutoKeepDeepEquality: KeepDeepEqualityCall<GridAuto> =
  GridAutoOrTemplateBaseKeepDeepEquality

export function GridContainerPropertiesKeepDeepEquality(): KeepDeepEqualityCall<GridContainerProperties> {
  return combine4EqualityCalls(
    (properties) => properties.gridTemplateColumns,
    nullableDeepEquality(GridTemplateKeepDeepEquality),
    (properties) => properties.gridTemplateRows,
    nullableDeepEquality(GridTemplateKeepDeepEquality),
    (properties) => properties.gridAutoColumns,
    nullableDeepEquality(GridAutoKeepDeepEquality),
    (properties) => properties.gridAutoRows,
    nullableDeepEquality(GridAutoKeepDeepEquality),
    gridContainerProperties,
  )
}

export const GridPositionValueKeepDeepEquality: KeepDeepEqualityCall<GridPositionValue> =
  combine1EqualityCall(
    (value) => value.numericalPosition,
    nullableDeepEquality(createCallWithTripleEquals<number>()),
    gridPositionValue,
  )

export const GridPositionKeepDeepEquality: KeepDeepEqualityCall<GridPosition> = (
  oldValue,
  newValue,
) => {
  if (typeof oldValue === 'string') {
    if (typeof newValue === 'string') {
      return createCallWithTripleEquals<GridPosition>()(oldValue, newValue)
    } else {
      return keepDeepEqualityResult(newValue, false)
    }
  } else {
    if (typeof newValue === 'string') {
      return keepDeepEqualityResult(newValue, false)
    } else {
      return GridPositionValueKeepDeepEquality(oldValue, newValue)
    }
  }
}

export function GridElementPropertiesKeepDeepEquality(): KeepDeepEqualityCall<GridElementProperties> {
  return combine4EqualityCalls(
    (properties) => properties.gridColumnStart,
    nullableDeepEquality(GridPositionKeepDeepEquality),
    (properties) => properties.gridColumnEnd,
    nullableDeepEquality(GridPositionKeepDeepEquality),
    (properties) => properties.gridRowStart,
    nullableDeepEquality(GridPositionKeepDeepEquality),
    (properties) => properties.gridRowEnd,
    nullableDeepEquality(GridPositionKeepDeepEquality),
    gridElementProperties,
  )
}

export function SpecialSizeMeasurementsKeepDeepEquality(): KeepDeepEqualityCall<SpecialSizeMeasurements> {
  return (oldSize, newSize) => {
    const offsetResult = LocalPointKeepDeepEquality(oldSize.offset, newSize.offset)
    const coordinateSystemBoundsResult = nullableDeepEquality(CanvasRectangleKeepDeepEquality)(
      oldSize.coordinateSystemBounds,
      newSize.coordinateSystemBounds,
    )
    const immediateParentBoundsResult = nullableDeepEquality(CanvasRectangleKeepDeepEquality)(
      oldSize.immediateParentBounds,
      newSize.immediateParentBounds,
    )
    const globalFrameWithTextContentResult = nullableDeepEquality(
      MaybeInfinityCanvasRectangleKeepDeepEquality,
    )(oldSize.globalFrameWithTextContent, newSize.globalFrameWithTextContent)
    const immediateParentProvidesLayoutResult =
      oldSize.immediateParentProvidesLayout === newSize.immediateParentProvidesLayout
    const closestOffsetParentPathResult = ElementPathKeepDeepEquality(
      oldSize.closestOffsetParentPath,
      newSize.closestOffsetParentPath,
    ).areEqual
    const usesParentBoundsResult = oldSize.usesParentBounds === newSize.usesParentBounds
    const parentLayoutSystemResult = oldSize.parentLayoutSystem === newSize.parentLayoutSystem
    const layoutSystemForChildrenResult = NullableStringKeepDeepEquality(
      oldSize.layoutSystemForChildren,
      newSize.layoutSystemForChildren,
    ).areEqual
    const providesBoundsForAbsoluteChildrenResult =
      oldSize.providesBoundsForAbsoluteChildren === newSize.providesBoundsForAbsoluteChildren
    const positionResult = oldSize.position === newSize.position
    const marginResult = SidesKeepDeepEquality(oldSize.margin, newSize.margin)
    const paddingResult = SidesKeepDeepEquality(oldSize.padding, newSize.padding)
    const naturalWidthResult = oldSize.naturalWidth === newSize.naturalWidth
    const naturalHeightResult = oldSize.naturalHeight === newSize.naturalHeight
    const clientWidthResult = oldSize.clientWidth === newSize.clientWidth
    const clientHeightResult = oldSize.clientHeight === newSize.clientHeight
    const parentFlexDirectionResult = oldSize.parentFlexDirection === newSize.parentFlexDirection
    const parentJustifyContentEquals = oldSize.parentJustifyContent === newSize.parentJustifyContent
    const parentFlexGapEquals = NumberKeepDeepEquality(oldSize.parentFlexGap, newSize.parentFlexGap)
    const parentPaddingEquals = SidesKeepDeepEquality(oldSize.parentPadding, newSize.parentPadding)
    const parentHugsOnMainAxisEquals = BooleanKeepDeepEquality(
      oldSize.parentHugsOnMainAxis,
      newSize.parentHugsOnMainAxis,
    )
    const gapEquals = NullableNumberKeepDeepEquality(oldSize.gap, newSize.gap).areEqual
    const flexDirectionResult = oldSize.flexDirection === newSize.flexDirection

    const justifyContentEquals = oldSize.justifyContent === newSize.justifyContent
    const alignItemsEquals = oldSize.alignItems === newSize.alignItems

    const displayEquals = oldSize.display === newSize.display
    const htmlElementNameEquals = oldSize.htmlElementName === newSize.htmlElementName
    const renderedChildrenCount = oldSize.renderedChildrenCount === newSize.renderedChildrenCount
    const globalContentBoxEquals = nullableDeepEquality(
      MaybeInfinityCanvasRectangleKeepDeepEquality,
    )(oldSize.globalContentBoxForChildren, newSize.globalContentBoxForChildren).areEqual
    const floatEquals = oldSize.float === newSize.float
    const hasPositionOffsetEquals = oldSize.hasPositionOffset === newSize.hasPositionOffset
    const textDirectionEquals = oldSize.parentTextDirection === newSize.parentTextDirection
    const hasTransformEquals = oldSize.hasTransform === newSize.hasTransform
    const borderRadiusEquals = nullableDeepEquality(SidesKeepDeepEquality)(
      oldSize.borderRadius,
      newSize.borderRadius,
    ).areEqual

    const fontSizeEquals = oldSize.fontSize === newSize.fontSize
    const fontWeightEquals = oldSize.fontWeight === newSize.fontWeight
    const fontStyleEquals = oldSize.fontStyle === newSize.fontStyle
    const textDecorationLineEquals = oldSize.textDecorationLine === newSize.textDecorationLine
    const textBoundsEqual = nullableDeepEquality(CanvasRectangleKeepDeepEquality)(
      oldSize.textBounds,
      newSize.textBounds,
    ).areEqual
    const computedHugPropertyEqual =
      oldSize.computedHugProperty.width === newSize.computedHugProperty.width &&
      oldSize.computedHugProperty.height === newSize.computedHugProperty.height

    const gridContainerPropertiesEqual = GridContainerPropertiesKeepDeepEquality()(
      oldSize.containerGridProperties,
      newSize.containerGridProperties,
    ).areEqual
    const gridElementPropertiesEqual = GridElementPropertiesKeepDeepEquality()(
      oldSize.elementGridProperties,
      newSize.elementGridProperties,
    ).areEqual

    const gridContainerPropertiesFromPropsEqual = GridContainerPropertiesKeepDeepEquality()(
      oldSize.containerGridPropertiesFromProps,
      newSize.containerGridPropertiesFromProps,
    ).areEqual
    const gridElementPropertiesFromPropsEqual = GridElementPropertiesKeepDeepEquality()(
      oldSize.elementGridPropertiesFromProps,
      newSize.elementGridPropertiesFromProps,
    ).areEqual

    const areEqual =
      offsetResult.areEqual &&
      coordinateSystemBoundsResult.areEqual &&
      immediateParentBoundsResult.areEqual &&
      globalFrameWithTextContentResult.areEqual &&
      immediateParentProvidesLayoutResult &&
      closestOffsetParentPathResult &&
      usesParentBoundsResult &&
      parentLayoutSystemResult &&
      layoutSystemForChildrenResult &&
      providesBoundsForAbsoluteChildrenResult &&
      positionResult &&
      marginResult.areEqual &&
      paddingResult.areEqual &&
      naturalWidthResult &&
      naturalHeightResult &&
      clientWidthResult &&
      clientHeightResult &&
      parentFlexDirectionResult &&
      parentJustifyContentEquals &&
      parentFlexGapEquals.areEqual &&
      parentPaddingEquals.areEqual &&
      parentHugsOnMainAxisEquals.areEqual &&
      gapEquals &&
      flexDirectionResult &&
      justifyContentEquals &&
      alignItemsEquals &&
      displayEquals &&
      htmlElementNameEquals &&
      renderedChildrenCount &&
      globalContentBoxEquals &&
      floatEquals &&
      hasPositionOffsetEquals &&
      textDirectionEquals &&
      hasTransformEquals &&
      borderRadiusEquals &&
      fontSizeEquals &&
      fontWeightEquals &&
      fontStyleEquals &&
      textDecorationLineEquals &&
      textBoundsEqual &&
      computedHugPropertyEqual &&
      gridContainerPropertiesEqual &&
      gridElementPropertiesEqual &&
      gridContainerPropertiesFromPropsEqual &&
      gridElementPropertiesFromPropsEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldSize, true)
    } else {
      const sizeMeasurements = specialSizeMeasurements(
        offsetResult.value,
        coordinateSystemBoundsResult.value,
        immediateParentBoundsResult.value,
        globalFrameWithTextContentResult.value,
        newSize.immediateParentProvidesLayout,
        newSize.closestOffsetParentPath,
        newSize.usesParentBounds,
        newSize.parentLayoutSystem,
        newSize.layoutSystemForChildren,
        newSize.providesBoundsForAbsoluteChildren,
        newSize.display,
        newSize.position,
        marginResult.value,
        paddingResult.value,
        newSize.naturalWidth,
        newSize.naturalHeight,
        newSize.clientWidth,
        newSize.clientHeight,
        newSize.parentFlexDirection,
        newSize.parentJustifyContent,
        newSize.parentFlexGap,
        newSize.parentPadding,
        newSize.parentHugsOnMainAxis,
        newSize.gap,
        newSize.flexDirection,
        newSize.justifyContent,
        newSize.alignItems,
        newSize.htmlElementName,
        newSize.renderedChildrenCount,
        newSize.globalContentBoxForChildren,
        newSize.float,
        newSize.hasPositionOffset,
        newSize.parentTextDirection,
        newSize.hasTransform,
        newSize.borderRadius,
        newSize.fontSize,
        newSize.fontWeight,
        newSize.fontStyle,
        newSize.textDecorationLine,
        newSize.textBounds,
        newSize.computedHugProperty,
        newSize.containerGridProperties,
        newSize.elementGridProperties,
        newSize.containerGridPropertiesFromProps,
        newSize.elementGridPropertiesFromProps,
        newSize.rowGap,
        newSize.columnGap,
      )
      return keepDeepEqualityResult(sizeMeasurements, false)
    }
  }
}

export const StyleAttributeMetadataEntryKeepDeepEquality: KeepDeepEqualityCall<
  StyleAttributeMetadataEntry
> = (oldValue: StyleAttributeMetadataEntry, newValue: StyleAttributeMetadataEntry) => {
  if (oldValue.fromStyleSheet === newValue.fromStyleSheet) {
    return keepDeepEqualityResult(oldValue, true)
  } else {
    return keepDeepEqualityResult(newValue, false)
  }
}

export const StyleAttributeMetadataKeepDeepEquality: KeepDeepEqualityCall<StyleAttributeMetadata> =
  objectDeepEquality(undefinableDeepEquality(StyleAttributeMetadataEntryKeepDeepEquality))

export const ElementInstanceMetadataPropsKeepDeepEquality: KeepDeepEqualityCall<any> =
  createCallWithShallowEquals()

const ActiveAndDefaultConditionValuesKeepDeepEquality: KeepDeepEqualityCall<ActiveAndDefaultConditionValues> =
  combine2EqualityCalls(
    (value) => value.active,
    BooleanKeepDeepEquality,
    (value) => value.default,
    BooleanKeepDeepEquality,
    (activeBranch: boolean, defaultBranch: boolean) => ({
      active: activeBranch,
      default: defaultBranch,
    }),
  )

const ConditionValueKeepDeepEquality: KeepDeepEqualityCall<ConditionValue> = unionDeepEquality(
  createCallWithTripleEquals<ConditionValue>(),
  ActiveAndDefaultConditionValuesKeepDeepEquality,
  (p): p is 'not-a-conditional' => p === 'not-a-conditional',
  (p): p is ActiveAndDefaultConditionValues => p !== 'not-a-conditional',
)

export const EarlyReturnResultKeepDeepEquality: KeepDeepEqualityCall<EarlyReturnResult> =
  combine1EqualityCall(
    (earlyReturn) => earlyReturn.result,
    createCallWithTripleEquals<unknown>(),
    earlyReturnResult,
  )

export const EarlyReturnVoidKeepDeepEquality: KeepDeepEqualityCall<EarlyReturnVoid> =
  createCallWithShallowEquals()

export const EarlyReturnKeepDeepEquality: KeepDeepEqualityCall<
  EarlyReturnResult | EarlyReturnVoid
> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'EARLY_RETURN_RESULT':
      if (newValue.type === oldValue.type) {
        return EarlyReturnResultKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EARLY_RETURN_VOID':
      if (newValue.type === oldValue.type) {
        return EarlyReturnVoidKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ElementInstanceMetadataKeepDeepEquality: KeepDeepEqualityCall<ElementInstanceMetadata> =
  combine16EqualityCalls(
    (metadata) => metadata.elementPath,
    ElementPathKeepDeepEquality,
    (metadata) => metadata.element,
    EitherKeepDeepEquality(createCallWithTripleEquals(), JSXElementChildKeepDeepEquality()),
    (metadata) => metadata.globalFrame,
    nullableDeepEquality(MaybeInfinityCanvasRectangleKeepDeepEquality),
    (metadata) => metadata.localFrame,
    nullableDeepEquality(MaybeInfinityLocalRectangleKeepDeepEquality),
    (metadata) => metadata.nonRoundedGlobalFrame,
    nullableDeepEquality(MaybeInfinityCanvasRectangleKeepDeepEquality),
    (metadata) => metadata.componentInstance,
    createCallWithTripleEquals(),
    (metadata) => metadata.isEmotionOrStyledComponent,
    createCallWithTripleEquals(),
    (metadata) => metadata.specialSizeMeasurements,
    SpecialSizeMeasurementsKeepDeepEquality(),
    (metadata) => metadata.computedStyle,
    nullableDeepEquality(objectDeepEquality(createCallWithTripleEquals())),
    (metadata) => metadata.attributeMetadatada,
    nullableDeepEquality(StyleAttributeMetadataKeepDeepEquality),
    (metadata) => metadata.label,
    nullableDeepEquality(createCallWithTripleEquals()),
    (metadata) => metadata.importInfo,
    nullableDeepEquality(ImportInfoKeepDeepEquality),
    (metadata) => metadata.conditionValue,
    ConditionValueKeepDeepEquality,
    (metadata) => metadata.textContent,
    nullableDeepEquality(StringKeepDeepEquality),
    (metadata) => metadata.earlyReturn,
    nullableDeepEquality(EarlyReturnKeepDeepEquality),
    (metadata) => metadata.assignedToProp,
    nullableDeepEquality(StringKeepDeepEquality),
    elementInstanceMetadata,
  )

export const ESCodeFileKeepDeepEquality: KeepDeepEqualityCall<ESCodeFile> = combine3EqualityCalls(
  (codeFile) => codeFile.fileContents,
  createCallWithTripleEquals(),
  (codeFile) => codeFile.origin,
  createCallWithTripleEquals(),
  (codeFile) => codeFile.fullPath,
  createCallWithTripleEquals(),
  esCodeFile,
)

export const ESRemoteDependencyPlaceholderKeepDeepEquality: KeepDeepEqualityCall<ESRemoteDependencyPlaceholder> =
  combine2EqualityCalls(
    (remoteDependencyPlaceholder) => remoteDependencyPlaceholder.url,
    createCallWithTripleEquals(),
    (remoteDependencyPlaceholder) => remoteDependencyPlaceholder.downloadStarted,
    createCallWithTripleEquals(),
    esRemoteDependencyPlaceholder,
  )

export const NodeModuleFileKeepDeepEquality: KeepDeepEqualityCall<NodeModuleFile> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'ES_CODE_FILE':
      if (newValue.type === oldValue.type) {
        return ESCodeFileKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'ES_REMOTE_DEPENDENCY_PLACEHOLDER':
      if (newValue.type === oldValue.type) {
        return ESRemoteDependencyPlaceholderKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ElementInstanceMetadataMapKeepDeepEquality: KeepDeepEqualityCall<ElementInstanceMetadataMap> =
  objectDeepEquality(ElementInstanceMetadataKeepDeepEquality)

export const ErrorMessageKeepDeepEquality: KeepDeepEqualityCall<ErrorMessage> =
  combine12EqualityCalls(
    (message) => message.fileName,
    createCallWithTripleEquals(),
    (message) => message.startLine,
    createCallWithTripleEquals(),
    (message) => message.startColumn,
    createCallWithTripleEquals(),
    (message) => message.endLine,
    createCallWithTripleEquals(),
    (message) => message.endColumn,
    createCallWithTripleEquals(),
    (message) => message.codeSnippet,
    createCallWithTripleEquals(),
    (message) => message.severity,
    createCallWithTripleEquals(),
    (message) => message.type,
    createCallWithTripleEquals(),
    (message) => message.message,
    createCallWithTripleEquals(),
    (message) => message.errorCode,
    createCallWithTripleEquals(),
    (message) => message.source,
    createCallWithTripleEquals(),
    (message) => message.passTime,
    createCallWithTripleEquals(),
    errorMessage,
  )

export const SingleFileBuildResultKeepDeepEquality: KeepDeepEqualityCall<SingleFileBuildResult> =
  combine3EqualityCalls(
    (buildResult) => buildResult.transpiledCode,
    nullableDeepEquality(createCallWithTripleEquals()),
    (buildResult) => buildResult.sourceMap,
    nullableDeepEquality(RawSourceMapKeepDeepEquality),
    (buildResult) => buildResult.errors,
    arrayDeepEquality(ErrorMessageKeepDeepEquality),
    singleFileBuildResult,
  )

export const MultiFileBuildResultKeepDeepEquality: KeepDeepEqualityCall<MultiFileBuildResult> =
  objectDeepEquality(SingleFileBuildResultKeepDeepEquality)

export const PackageStatusKeepDeepEquality: KeepDeepEqualityCall<PackageStatus> =
  createCallWithTripleEquals()

export const GithubFileStatusKeepDeepEquality: KeepDeepEqualityCall<GithubFileStatus> =
  createCallWithTripleEquals()

export const PackageDetailsKeepDeepEquality: KeepDeepEqualityCall<PackageDetails> =
  combine1EqualityCall((details) => details.status, PackageStatusKeepDeepEquality, packageDetails)

export const PackageStatusMapKeepDeepEquality: KeepDeepEqualityCall<PackageStatusMap> =
  objectDeepEquality(PackageDetailsKeepDeepEquality)

export const EditorStateNodeModulesKeepDeepEquality: KeepDeepEqualityCall<EditorStateNodeModules> =
  combine4EqualityCalls(
    (nodeModules) => nodeModules.skipDeepFreeze,
    createCallWithTripleEquals(),
    (nodeModules) => nodeModules.files,
    objectDeepEquality(NodeModuleFileKeepDeepEquality),
    (nodeModules) => nodeModules.projectFilesBuildResults,
    MultiFileBuildResultKeepDeepEquality,
    (nodeModules) => nodeModules.packageStatus,
    PackageStatusMapKeepDeepEquality,
    editorStateNodeModules,
  )

export const EditorStateLeftMenuKeepDeepEquality: KeepDeepEqualityCall<EditorStateLeftMenu> =
  combine2EqualityCalls(
    (esLeftMenu) => esLeftMenu.selectedTab,
    createCallWithTripleEquals(),
    (esLeftMenu) => esLeftMenu.visible,
    createCallWithTripleEquals(),
    editorStateLeftMenu,
  )

export const EditorStateRightMenuKeepDeepEquality: KeepDeepEqualityCall<EditorStateRightMenu> =
  combine2EqualityCalls(
    (esRightMenu) => esRightMenu.selectedTab,
    createCallWithTripleEquals(),
    (esRightMenu) => esRightMenu.visible,
    createCallWithTripleEquals(),
    editorStateRightMenu,
  )

export const EditorStateInterfaceDesignerKeepDeepEquality: KeepDeepEqualityCall<EditorStateInterfaceDesigner> =
  combine2EqualityCalls(
    (designer) => designer.codePaneVisible,
    createCallWithTripleEquals(),
    (designer) => designer.additionalControls,
    createCallWithTripleEquals(),
    editorStateInterfaceDesigner,
  )

export const EditorStateCanvasTextEditorKeepDeepEquality: KeepDeepEqualityCall<EditorStateCanvasTextEditor> =
  combine2EqualityCalls(
    (editor) => editor.elementPath,
    ElementPathKeepDeepEquality,
    (editor) => editor.triggerMousePosition,
    nullableDeepEquality(WindowPointKeepDeepEquality),
    editorStateCanvasTextEditor,
  )

export const EditorStateCanvasTransientPropertyKeepDeepEquality: KeepDeepEqualityCall<EditorStateCanvasTransientProperty> =
  combine2EqualityCalls(
    (property) => property.elementPath,
    ElementPathKeepDeepEquality,
    (property) => property.attributesToUpdate,
    objectDeepEquality(JSExpressionKeepDeepEqualityCall),
    editorStateCanvasTransientProperty,
  )

export const XAxisGuidelineKeepDeepEquality: KeepDeepEqualityCall<XAxisGuideline> =
  combine3EqualityCalls(
    (guideline) => guideline.x,
    createCallWithTripleEquals(),
    (guideline) => guideline.yTop,
    createCallWithTripleEquals(),
    (guideline) => guideline.yBottom,
    createCallWithTripleEquals(),
    xAxisGuideline,
  )

export const YAxisGuidelineKeepDeepEquality: KeepDeepEqualityCall<YAxisGuideline> =
  combine3EqualityCalls(
    (guideline) => guideline.y,
    createCallWithTripleEquals(),
    (guideline) => guideline.xLeft,
    createCallWithTripleEquals(),
    (guideline) => guideline.xRight,
    createCallWithTripleEquals(),
    yAxisGuideline,
  )

export const CornerGuidelineKeepDeepEquality: KeepDeepEqualityCall<CornerGuideline> =
  combine4EqualityCalls(
    (guideline) => guideline.x,
    createCallWithTripleEquals(),
    (guideline) => guideline.y,
    createCallWithTripleEquals(),
    (guideline) => guideline.xMovement,
    createCallWithTripleEquals(),
    (guideline) => guideline.yMovement,
    createCallWithTripleEquals(),
    cornerGuideline,
  )

export const GuidelineKeepDeepEquality: KeepDeepEqualityCall<Guideline> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'XAxisGuideline':
      if (newValue.type === oldValue.type) {
        return XAxisGuidelineKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'YAxisGuideline':
      if (newValue.type === oldValue.type) {
        return YAxisGuidelineKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'CornerGuideline':
      if (newValue.type === oldValue.type) {
        return CornerGuidelineKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const GuidelineWithSnappingVectorAndPointsOfRelevanceKeepDeepEquality: KeepDeepEqualityCall<GuidelineWithSnappingVectorAndPointsOfRelevance> =
  combine3EqualityCalls(
    (guideline) => guideline.guideline,
    GuidelineKeepDeepEquality,
    (guideline) => guideline.snappingVector,
    CanvasPointKeepDeepEquality,
    (guideline) => guideline.pointsOfRelevance,
    arrayDeepEquality(CanvasPointKeepDeepEquality),
    guidelineWithSnappingVectorAndPointsOfRelevance,
  )

export const DragToMoveIndicatorFlagsKeepDeepEquality: KeepDeepEqualityCall<DragToMoveIndicatorFlags> =
  combine4EqualityCalls(
    (indicatorFlag) => indicatorFlag.showIndicator,
    BooleanKeepDeepEquality,
    (indicatorFlag) => indicatorFlag.dragType,
    createCallWithTripleEquals<'absolute' | 'static' | 'none'>(),
    (indicatorFlag) => indicatorFlag.reparent,
    createCallWithTripleEquals<'same-component' | 'different-component' | 'none'>(),
    (indicatorFlag) => indicatorFlag.ancestor,
    BooleanKeepDeepEquality,
    dragToMoveIndicatorFlags,
  )

export const EditorStateCanvasControlsKeepDeepEquality: KeepDeepEqualityCall<EditorStateCanvasControls> =
  combine8EqualityCalls(
    (controls) => controls.snappingGuidelines,
    arrayDeepEquality(GuidelineWithSnappingVectorAndPointsOfRelevanceKeepDeepEquality),
    (controls) => controls.outlineHighlights,
    arrayDeepEquality(CanvasRectangleKeepDeepEquality),
    (controls) => controls.strategyIntendedBounds,
    arrayDeepEquality(CanvasFrameAndTargetKeepDeepEquality),
    (controls) => controls.flexReparentTargetLines,
    arrayDeepEquality(CanvasRectangleKeepDeepEquality),
    (controls) => controls.parentHighlightPaths,
    nullableDeepEquality(arrayDeepEquality(ElementPathKeepDeepEquality)),
    (controls) => controls.reparentedToPaths,
    ElementPathArrayKeepDeepEquality,
    (controls) => controls.dragToMoveIndicatorFlags,
    DragToMoveIndicatorFlagsKeepDeepEquality,
    (controls) => controls.parentOutlineHighlight,
    nullableDeepEquality(ElementPathKeepDeepEquality),
    editorStateCanvasControls,
  )

export const ModifiersKeepDeepEquality: KeepDeepEqualityCall<Modifiers> = combine4EqualityCalls(
  (modifiers) => modifiers.alt,
  createCallWithTripleEquals(),
  (modifiers) => modifiers.cmd,
  createCallWithTripleEquals(),
  (modifiers) => modifiers.ctrl,
  createCallWithTripleEquals(),
  (modifiers) => modifiers.shift,
  createCallWithTripleEquals(),
  (alt, cmd, ctrl, shift) => {
    return {
      alt: alt,
      cmd: cmd,
      ctrl: ctrl,
      shift: shift,
    }
  },
)

export const DragInteractionDataKeepDeepEquality: KeepDeepEqualityCall<DragInteractionData> =
  combine9EqualityCalls(
    (data) => data.dragStart,
    CanvasPointKeepDeepEquality,
    (data) => data.drag,
    nullableDeepEquality(CanvasPointKeepDeepEquality),
    (data) => data.prevDrag,
    createCallWithTripleEquals(),
    (data) => data.originalDragStart,
    CanvasPointKeepDeepEquality,
    (data) => data.modifiers,
    ModifiersKeepDeepEquality,
    (data) => data.hasMouseMoved,
    BooleanKeepDeepEquality,
    (data) => data._accumulatedMovement,
    CanvasPointKeepDeepEquality,
    (data) => data.spacePressed,
    BooleanKeepDeepEquality,
    (data) => data.zeroDragPermitted,
    createCallWithTripleEquals<ZeroDragPermitted>(),
    (
      dragStart,
      drag,
      prevDrag,
      originalDragStart,
      modifiers,
      hasMouseMoved,
      accumulatedMovement,
      spacePressed,
      zeroDragPermitted,
    ) => {
      return {
        type: 'DRAG',
        dragStart: dragStart,
        drag: drag,
        prevDrag: prevDrag,
        originalDragStart: originalDragStart,
        modifiers: modifiers,
        hasMouseMoved: hasMouseMoved,
        _accumulatedMovement: accumulatedMovement,
        spacePressed: spacePressed,
        zeroDragPermitted: zeroDragPermitted,
      }
    },
  )

export const HoverInteractionDataKeepDeepEquality: KeepDeepEqualityCall<HoverInteractionData> =
  combine3EqualityCalls(
    (data) => data.point,
    CanvasPointKeepDeepEquality,
    (data) => data.modifiers,
    ModifiersKeepDeepEquality,
    (data) => data.zeroDragPermitted,
    createCallWithTripleEquals<ZeroDragPermitted>(),
    (point, modifiers, zeroDragPermitted) => {
      return {
        type: 'HOVER',
        point: point,
        modifiers: modifiers,
        zeroDragPermitted: zeroDragPermitted,
      }
    },
  )

export const ImportAliasKeepDeepEquality: KeepDeepEqualityCall<ImportAlias> = combine2EqualityCalls(
  (alias) => alias.name,
  StringKeepDeepEquality,
  (alias) => alias.alias,
  StringKeepDeepEquality,
  importAlias,
)

export const ImportDetailsKeepDeepEquality: KeepDeepEqualityCall<ImportDetails> =
  combine3EqualityCalls(
    (details) => details.importedWithName,
    NullableStringKeepDeepEquality,
    (details) => details.importedFromWithin,
    arrayDeepEquality(ImportAliasKeepDeepEquality),
    (details) => details.importedAs,
    NullableStringKeepDeepEquality,
    importDetails,
  )

export const ElementPasteKeepDeepEquality: KeepDeepEqualityCall<ElementPaste> =
  combine3EqualityCalls(
    (c) => c.element,
    JSXElementChildKeepDeepEquality(),
    (c) => c.importsToAdd,
    objectDeepEquality(ImportDetailsKeepDeepEquality),
    (c) => c.originalElementPath,
    ElementPathKeepDeepEquality,
    elementPaste,
  )

export const ElementPasteWithMetadataKeepDeepEquality: KeepDeepEqualityCall<ElementPasteWithMetadata> =
  combine2EqualityCalls(
    (c) => c.elements,
    arrayDeepEquality(ElementPasteKeepDeepEquality),
    (c) => c.targetOriginalContextMetadata,
    ElementInstanceMetadataMapKeepDeepEquality,
    (elements, targetOriginalContextMetadata) => ({ elements, targetOriginalContextMetadata }),
  )

export const KeyStateKeepDeepEquality: KeepDeepEqualityCall<KeyState> = combine2EqualityCalls(
  (keyState) => keyState.keysPressed,
  createCallWithDeepEquals(),
  (keyState) => keyState.modifiers,
  ModifiersKeepDeepEquality,
  (keysPressed, modifiers) => {
    return {
      keysPressed: keysPressed,
      modifiers: modifiers,
    }
  },
)

export const KeyboardInteractionDataKeepDeepEquality: KeepDeepEqualityCall<KeyboardInteractionData> =
  combine1EqualityCall(
    (data) => data.keyStates,
    arrayDeepEquality(KeyStateKeepDeepEquality),
    (keyStates) => {
      return {
        type: 'KEYBOARD',
        keyStates: keyStates,
      }
    },
  )

export const InputDataKeepDeepEquality: KeepDeepEqualityCall<InputData> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'DRAG':
      if (newValue.type === oldValue.type) {
        return DragInteractionDataKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'KEYBOARD':
      if (newValue.type === oldValue.type) {
        return KeyboardInteractionDataKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'HOVER':
      if (newValue.type === oldValue.type) {
        return HoverInteractionDataKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const EdgePositionKeepDeepEquality: KeepDeepEqualityCall<EdgePosition> =
  combine2EqualityCalls(
    (edge) => edge.x,
    createCallWithTripleEquals(),
    (edge) => edge.y,
    createCallWithTripleEquals(),
    edgePosition,
  )
boundingArea() // this is here to break if the definition of boundingArea changes
export const BoundingAreaKeepDeepEquality: KeepDeepEqualityCall<BoundingArea> = (oldValue, _) => {
  return keepDeepEqualityResult(oldValue, true)
}

export const ResizeHandleKeepDeepEquality: KeepDeepEqualityCall<ResizeHandle> =
  combine1EqualityCall((handle) => handle.edgePosition, EdgePositionKeepDeepEquality, resizeHandle)

// This will break should the definition of `FlexGapHandle` change.
flexGapHandle()
export const FlexGapHandleKeepDeepEquality: KeepDeepEqualityCall<FlexGapHandle> = (
  oldValue,
  newValue,
) => {
  return keepDeepEqualityResult(oldValue, true)
}

// This will break should the definition of `KeyboardCatcherControl` change.
keyboardCatcherControl()
export const KeyboardCatcherControlKeepDeepEquality: KeepDeepEqualityCall<
  KeyboardCatcherControl
> = (oldValue, newValue) => {
  return keepDeepEqualityResult(oldValue, true)
}

export const PaddingResizeHandleKeepDeepEquality: KeepDeepEqualityCall<PaddingResizeHandle> = (
  oldValue,
  newValue,
) => {
  return keepDeepEqualityResult(oldValue, true)
}

// This will break should the definition of `ReorderSlider` change.
reorderSlider()
export const ReorderSliderKeepDeepEquality: KeepDeepEqualityCall<ReorderSlider> = (
  oldValue,
  newValue,
) => {
  return keepDeepEqualityResult(oldValue, true)
}

export const BorderRadiusResizeHandleKeepDeepEquality: KeepDeepEqualityCall<
  BorderRadiusResizeHandle
> = (oldValue, newValue) => {
  return keepDeepEqualityResult(oldValue, true)
}

export const GridCellHandleKeepDeepEquality: KeepDeepEqualityCall<GridCellHandle> =
  combine1EqualityCall(
    (handle) => handle.id,
    createCallWithTripleEquals<string>(),
    (id) => gridCellHandle({ id }),
  )

export const GridAxisHandleKeepDeepEquality: KeepDeepEqualityCall<GridAxisHandle> =
  combine2EqualityCalls(
    (handle) => handle.axis,
    createCallWithTripleEquals(),
    (handle) => handle.columnOrRow,
    createCallWithTripleEquals<number>(),
    gridAxisHandle,
  )

export const GridResizeHandleKeepDeepEquality: KeepDeepEqualityCall<GridResizeHandle> =
  combine2EqualityCalls(
    (handle) => handle.id,
    createCallWithTripleEquals<string>(),
    (handle) => handle.edge,
    createCallWithTripleEquals<GridResizeEdge>(),
    gridResizeHandle,
  )

export const CanvasControlTypeKeepDeepEquality: KeepDeepEqualityCall<CanvasControlType> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'BOUNDING_AREA':
      if (newValue.type === oldValue.type) {
        return BoundingAreaKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'RESIZE_HANDLE':
      if (newValue.type === oldValue.type) {
        return ResizeHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'FLEX_GAP_HANDLE':
      if (newValue.type === oldValue.type) {
        return FlexGapHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'KEYBOARD_CATCHER_CONTROL':
      if (newValue.type === oldValue.type) {
        return KeyboardCatcherControlKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'PADDING_RESIZE_HANDLE':
      if (newValue.type === oldValue.type) {
        return PaddingResizeHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'REORDER_SLIDER':
      if (newValue.type === oldValue.type) {
        return ReorderSliderKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'BORDER_RADIUS_RESIZE_HANDLE':
      if (newValue.type === oldValue.type) {
        return BorderRadiusResizeHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'GRID_CELL_HANDLE':
      if (newValue.type === oldValue.type) {
        return GridCellHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'GRID_AXIS_HANDLE':
      if (newValue.type === oldValue.type) {
        return GridAxisHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'GRID_RESIZE_HANDLE':
      if (newValue.type === oldValue.type) {
        return GridResizeHandleKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export function ElementPathTreesKeepDeepEquality(): KeepDeepEqualityCall<ElementPathTrees> {
  return objectDeepEquality(ElementPathTreeKeepDeepEquality)
}

export function ElementPathTreeKeepDeepEquality(
  oldValue: ElementPathTree,
  newValue: ElementPathTree,
): KeepDeepEqualityResult<ElementPathTree> {
  return combine3EqualityCalls(
    (pathTree) => pathTree.path,
    ElementPathKeepDeepEquality,
    (pathTree) => pathTree.pathString,
    createCallWithTripleEquals<string>(),
    (pathTree) => pathTree.children,
    arrayDeepEquality(ElementPathTreeKeepDeepEquality),
    elementPathTree,
  )(oldValue, newValue)
}

export const VariablesInScopeKeepDeepEquality: KeepDeepEqualityCall<VariablesInScope> =
  objectDeepEquality(objectDeepEquality(createCallFromIntrospectiveKeepDeep()))

export const InteractionSessionKeepDeepEquality: KeepDeepEqualityCall<InteractionSession> =
  combine11EqualityCalls(
    (session) => session.interactionData,
    InputDataKeepDeepEquality,
    (session) => session.activeControl,
    CanvasControlTypeKeepDeepEquality,
    (session) => session.lastInteractionTime,
    createCallWithTripleEquals(),
    (session) => session.latestMetadata,
    ElementInstanceMetadataMapKeepDeepEquality,
    (sesssion) => sesssion.userPreferredStrategy,
    nullableDeepEquality(createCallWithTripleEquals()),
    (session) => session.startedAt,
    createCallWithTripleEquals(),
    (session) => session.latestAllElementProps,
    createCallFromIntrospectiveKeepDeep(),
    (session) => session.latestVariablesInScope,
    VariablesInScopeKeepDeepEquality,
    (session) => session.updatedTargetPaths,
    objectDeepEquality(ElementPathKeepDeepEquality),
    (session) => session.aspectRatioLock,
    nullableDeepEquality(createCallWithTripleEquals()),
    (session) => session.latestElementPathTree,
    ElementPathTreesKeepDeepEquality(),
    interactionSession,
  )

export const OriginalPathKeepDeepEquality: KeepDeepEqualityCall<OriginalPath> =
  combine2EqualityCalls(
    (original) => original.originalTP,
    ElementPathKeepDeepEquality,
    (original) => original.currentTP,
    ElementPathKeepDeepEquality,
    originalPath,
  )

export const DuplicationStateKeepDeepEquality: KeepDeepEqualityCall<DuplicationState> =
  combine1EqualityCall(
    (state) => state.duplicateRoots,
    arrayDeepEquality(OriginalPathKeepDeepEquality),
    duplicationState,
  )

export const ImageBlogKeepDeepEquality: KeepDeepEqualityCall<ImageBlob> = combine1EqualityCall(
  (blob) => blob.base64,
  createCallWithTripleEquals(),
  imageBlob,
)

export const UIFileBase64BlobsKeepDeepEquality: KeepDeepEqualityCall<UIFileBase64Blobs> =
  objectDeepEquality(ImageBlogKeepDeepEquality)

export const CanvasBase64BlobsKeepDeepEquality: KeepDeepEqualityCall<CanvasBase64Blobs> =
  objectDeepEquality(UIFileBase64BlobsKeepDeepEquality)

export const DesignerFileKeepDeepEquality: KeepDeepEqualityCall<DesignerFile> =
  combine1EqualityCall((file) => file.filename, createCallWithTripleEquals(), designerFile)

export const ResizeOptionsKeepDeepEquality: KeepDeepEqualityCall<ResizeOptions> =
  combine2EqualityCalls(
    (options) => options.propertyTargetOptions,
    arrayDeepEquality(createCallWithTripleEquals()),
    (options) => options.propertyTargetSelectedIndex,
    createCallWithTripleEquals(),
    resizeOptions,
  )

export const EditorStateCanvasKeepDeepEquality: KeepDeepEqualityCall<EditorStateCanvas> = (
  oldValue,
  newValue,
) => {
  if (oldValue === newValue) {
    return keepDeepEqualityResult(oldValue, true)
  }

  const elementsToRerenderResult = ElementsToRerenderKeepDeepEquality(
    oldValue.elementsToRerender,
    newValue.elementsToRerender,
  )

  const interactionSessionResult = nullableDeepEquality(InteractionSessionKeepDeepEquality)(
    oldValue.interactionSession,
    newValue.interactionSession,
  )
  const scaleResult = NumberKeepDeepEquality(oldValue.scale, newValue.scale)
  const snappingThresholdResult = NumberKeepDeepEquality(
    oldValue.snappingThreshold,
    newValue.snappingThreshold,
  )
  const realCanvasOffsetResult = CanvasPointKeepDeepEquality(
    oldValue.realCanvasOffset,
    newValue.realCanvasOffset,
  )
  const roundedCanvasOffsetResult = CanvasPointKeepDeepEquality(
    oldValue.roundedCanvasOffset,
    newValue.roundedCanvasOffset,
  )
  const textEditorResult = nullableDeepEquality(EditorStateCanvasTextEditorKeepDeepEquality)(
    oldValue.textEditor,
    newValue.textEditor,
  )
  const selectionControlsVisibleResult = BooleanKeepDeepEquality(
    oldValue.selectionControlsVisible,
    newValue.selectionControlsVisible,
  )
  const cursorResult = nullableDeepEquality(createCallWithTripleEquals<CSSCursor>())(
    oldValue.cursor,
    newValue.cursor,
  )
  const duplicationStateResult = nullableDeepEquality(DuplicationStateKeepDeepEquality)(
    oldValue.duplicationState,
    newValue.duplicationState,
  )
  const base64BlobsResult = CanvasBase64BlobsKeepDeepEquality(
    oldValue.base64Blobs,
    newValue.base64Blobs,
  )
  const mountCountResult = NumberKeepDeepEquality(oldValue.mountCount, newValue.mountCount)
  const canvasContentInvalidateCountResult = NumberKeepDeepEquality(
    oldValue.canvasContentInvalidateCount,
    newValue.canvasContentInvalidateCount,
  )
  const domWalkerInvalidateCountResult = NumberKeepDeepEquality(
    oldValue.domWalkerInvalidateCount,
    newValue.domWalkerInvalidateCount,
  )
  const openFileResult = nullableDeepEquality(DesignerFileKeepDeepEquality)(
    oldValue.openFile,
    newValue.openFile,
  )
  const scrollAnimationResult = BooleanKeepDeepEquality(
    oldValue.scrollAnimation,
    newValue.scrollAnimation,
  )
  const transientPropertiesResult = nullableDeepEquality(
    objectDeepEquality(EditorStateCanvasTransientPropertyKeepDeepEquality),
  )(oldValue.transientProperties, newValue.transientProperties)
  const resizeOptionsResult = ResizeOptionsKeepDeepEquality(
    oldValue.resizeOptions,
    newValue.resizeOptions,
  )
  const domWalkerAdditionalElementsToUpdateResult = arrayDeepEquality(ElementPathKeepDeepEquality)(
    oldValue.domWalkerAdditionalElementsToUpdate,
    newValue.domWalkerAdditionalElementsToUpdate,
  )
  const controlsResult = EditorStateCanvasControlsKeepDeepEquality(
    oldValue.controls,
    newValue.controls,
  )

  const areEqual =
    elementsToRerenderResult.areEqual &&
    interactionSessionResult.areEqual &&
    scaleResult.areEqual &&
    snappingThresholdResult.areEqual &&
    realCanvasOffsetResult.areEqual &&
    roundedCanvasOffsetResult.areEqual &&
    textEditorResult.areEqual &&
    selectionControlsVisibleResult.areEqual &&
    cursorResult.areEqual &&
    duplicationStateResult.areEqual &&
    base64BlobsResult.areEqual &&
    mountCountResult.areEqual &&
    canvasContentInvalidateCountResult.areEqual &&
    domWalkerInvalidateCountResult.areEqual &&
    openFileResult.areEqual &&
    scrollAnimationResult.areEqual &&
    transientPropertiesResult.areEqual &&
    resizeOptionsResult.areEqual &&
    domWalkerAdditionalElementsToUpdateResult.areEqual &&
    controlsResult.areEqual
  if (areEqual) {
    return keepDeepEqualityResult(oldValue, true)
  } else {
    const newDeepValue = editorStateCanvas(
      elementsToRerenderResult.value,
      interactionSessionResult.value,
      scaleResult.value,
      snappingThresholdResult.value,
      realCanvasOffsetResult.value,
      roundedCanvasOffsetResult.value,
      textEditorResult.value,
      selectionControlsVisibleResult.value,
      cursorResult.value,
      duplicationStateResult.value,
      base64BlobsResult.value,
      mountCountResult.value,
      canvasContentInvalidateCountResult.value,
      domWalkerInvalidateCountResult.value,
      openFileResult.value,
      scrollAnimationResult.value,
      transientPropertiesResult.value,
      resizeOptionsResult.value,
      domWalkerAdditionalElementsToUpdateResult.value,
      controlsResult.value,
    )
    return keepDeepEqualityResult(newDeepValue, false)
  }
}

export const ExportVariablesWithModifierKeepDeepEquality: KeepDeepEqualityCall<ExportVariablesWithModifier> =
  combine1EqualityCall(
    (exportVars) => exportVars.variables,
    arrayDeepEquality(StringKeepDeepEquality),
    exportVariablesWithModifier,
  )

export const ExportFunctionKeepDeepEquality: KeepDeepEqualityCall<ExportFunction> =
  combine1EqualityCall((expFn) => expFn.functionName, StringKeepDeepEquality, exportFunction)

export const ExportClassKeepDeepEquality: KeepDeepEqualityCall<ExportClass> = combine1EqualityCall(
  (expClass) => expClass.className,
  StringKeepDeepEquality,
  exportClass,
)

export const ExportVariableKeepDeepEquality: KeepDeepEqualityCall<ExportVariable> =
  combine2EqualityCalls(
    (expVar) => expVar.variableName,
    StringKeepDeepEquality,
    (expVar) => expVar.variableAlias,
    NullableStringKeepDeepEquality,
    exportVariable,
  )

export const ExportVariablesKeepDeepEquality: KeepDeepEqualityCall<ExportVariables> =
  combine1EqualityCall(
    (expVars) => expVars.variables,
    arrayDeepEquality(ExportVariableKeepDeepEquality),
    exportVariables,
  )

export const ExportDestructuredAssignmentKeepDeepEquality: KeepDeepEqualityCall<ExportDestructuredAssignment> =
  combine1EqualityCall(
    (expAssign) => expAssign.variables,
    arrayDeepEquality(ExportVariableKeepDeepEquality),
    exportDestructuredAssignment,
  )

export const ExportDefaultFunctionOrClassKeepDeepEquality: KeepDeepEqualityCall<ExportDefaultFunctionOrClass> =
  combine1EqualityCall(
    (expFnOrClass) => expFnOrClass.name,
    NullableStringKeepDeepEquality,
    exportDefaultFunctionOrClass,
  )

export const ExportIdentifierKeepDeepEquality: KeepDeepEqualityCall<ExportDefaultIdentifier> =
  combine1EqualityCall((expIdent) => expIdent.name, StringKeepDeepEquality, exportDefaultIdentifier)

export const ReexportWildcardKeepDeepEquality: KeepDeepEqualityCall<ReexportWildcard> =
  combine2EqualityCalls(
    (reex) => reex.reexportedModule,
    StringKeepDeepEquality,
    (reex) => reex.namespacedVariable,
    NullableStringKeepDeepEquality,
    reexportWildcard,
  )

export const ReexportVariablesKeepDeepEquality: KeepDeepEqualityCall<ReexportVariables> =
  combine2EqualityCalls(
    (reex) => reex.reexportedModule,
    StringKeepDeepEquality,
    (reex) => reex.variables,
    arrayDeepEquality(ExportVariableKeepDeepEquality),
    reexportVariables,
  )

export const ExportDetailKeepDeepEquality: KeepDeepEqualityCall<ExportDetail> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'EXPORT_VARIABLES_WITH_MODIFIER':
      if (newValue.type === oldValue.type) {
        return ExportVariablesWithModifierKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EXPORT_FUNCTION':
      if (newValue.type === oldValue.type) {
        return ExportFunctionKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EXPORT_CLASS':
      if (newValue.type === oldValue.type) {
        return ExportClassKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EXPORT_VARIABLES':
      if (newValue.type === oldValue.type) {
        return ExportVariablesKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EXPORT_DESTRUCTURED_ASSIGNMENT':
      if (newValue.type === oldValue.type) {
        return ExportDestructuredAssignmentKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EXPORT_DEFAULT_FUNCTION_OR_CLASS':
      if (newValue.type === oldValue.type) {
        return ExportDefaultFunctionOrClassKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'EXPORT_DEFAULT_IDENTIFIER':
      if (newValue.type === oldValue.type) {
        return ExportIdentifierKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'REEXPORT_WILDCARD':
      if (newValue.type === oldValue.type) {
        return ReexportWildcardKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'REEXPORT_VARIABLES':
      if (newValue.type === oldValue.type) {
        return ReexportVariablesKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ParsedJSONFailureKeepDeepEquality: KeepDeepEqualityCall<ParsedJSONFailure> =
  combine6EqualityCalls(
    (failure) => failure.codeSnippet,
    StringKeepDeepEquality,
    (failure) => failure.reason,
    StringKeepDeepEquality,
    (failure) => failure.startLine,
    NumberKeepDeepEquality,
    (failure) => failure.startCol,
    NumberKeepDeepEquality,
    (failure) => failure.endLine,
    NumberKeepDeepEquality,
    (failure) => failure.endCol,
    NumberKeepDeepEquality,
    parsedJSONFailure,
  )

export const ParseFailureKeepDeepEquality: KeepDeepEqualityCall<ParseFailure> =
  combine4EqualityCalls(
    (failure) => failure.diagnostics,
    nullableDeepEquality(arrayDeepEquality(ErrorMessageKeepDeepEquality)),
    (failure) => failure.parsedJSONFailure,
    nullableDeepEquality(ParsedJSONFailureKeepDeepEquality),
    (failure) => failure.errorMessage,
    NullableStringKeepDeepEquality,
    (failure) => failure.errorMessages,
    arrayDeepEquality(ErrorMessageKeepDeepEquality),
    parseFailure,
  )

export const HighlightBoundsKeepDeepEquality: KeepDeepEqualityCall<HighlightBounds> =
  combine5EqualityCalls(
    (bounds) => bounds.startLine,
    NumberKeepDeepEquality,
    (bounds) => bounds.startCol,
    NumberKeepDeepEquality,
    (bounds) => bounds.endLine,
    NumberKeepDeepEquality,
    (bounds) => bounds.endCol,
    NumberKeepDeepEquality,
    (bounds) => bounds.uid,
    StringKeepDeepEquality,
    highlightBounds,
  )

export const BoundsKeepDeepEquality: KeepDeepEqualityCall<Bounds> = combine4EqualityCalls(
  (bounds) => bounds.startLine,
  NumberKeepDeepEquality,
  (bounds) => bounds.startCol,
  NumberKeepDeepEquality,
  (bounds) => bounds.endLine,
  NumberKeepDeepEquality,
  (bounds) => bounds.endCol,
  NumberKeepDeepEquality,
  (startLine, startCol, endLine, endCol) => ({
    startLine,
    startCol,
    endLine,
    endCol,
  }),
)

export const ComponentDescriptorPropertiesBoundsKeepDeepEquality: KeepDeepEqualityCall<ComponentDescriptorPropertiesBounds> =
  objectDeepEquality(BoundsKeepDeepEquality)

export const ComponentDescriptorBoundsKeepDeepEquality: KeepDeepEqualityCall<ComponentDescriptorBounds> =
  combine2EqualityCalls(
    (descriptorBounds) => descriptorBounds.bounds,
    BoundsKeepDeepEquality,
    (descriptorBounds) => descriptorBounds.properties,
    ComponentDescriptorPropertiesBoundsKeepDeepEquality,
    (bounds, properties) => ({
      bounds,
      properties,
    }),
  )

export const HighlightBoundsForUidsKeepDeepEquality: KeepDeepEqualityCall<HighlightBoundsForUids> =
  objectDeepEquality(HighlightBoundsKeepDeepEquality)

export const ParseSuccessKeepDeepEquality: KeepDeepEqualityCall<ParseSuccess> =
  combine7EqualityCalls(
    (success) => success.imports,
    objectDeepEquality(ImportDetailsKeepDeepEquality),
    (success) => success.topLevelElements,
    arrayDeepEquality(TopLevelElementKeepDeepEquality),
    (success) => success.highlightBounds,
    HighlightBoundsForUidsKeepDeepEquality,
    (success) => success.jsxFactoryFunction,
    NullableStringKeepDeepEquality,
    (success) => success.combinedTopLevelArbitraryBlock,
    nullableDeepEquality(ArbitraryJSBlockKeepDeepEquality()),
    (success) => success.exportsDetail,
    arrayDeepEquality(ExportDetailKeepDeepEquality),
    (success) => success.fullHighlightBounds,
    HighlightBoundsForUidsKeepDeepEquality,
    parseSuccess,
  )

export const UnparsedKeepDeepEquality: KeepDeepEqualityCall<Unparsed> = (oldValue, newValue) => {
  // This may not trip if the definition of `Unparsed` changes.
  return keepDeepEqualityResult(oldValue, true)
}

export const ParsedTextFileKeepDeepEquality: KeepDeepEqualityCall<ParsedTextFile> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'PARSE_FAILURE':
      if (newValue.type === oldValue.type) {
        return ParseFailureKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'PARSE_SUCCESS':
      if (newValue.type === oldValue.type) {
        return ParseSuccessKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'UNPARSED':
      if (newValue.type === oldValue.type) {
        return UnparsedKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

// Here so that this breaks the build if the definition of `Directory` changes.
directory()
export const DirectoryKeepDeepEquality: KeepDeepEqualityCall<Directory> = (oldValue, newValue) => {
  return keepDeepEqualityResult(oldValue, true)
}

export const TextFileContentsKeepDeepEquality: KeepDeepEqualityCall<TextFileContents> =
  combine3EqualityCalls(
    (contents) => contents.code,
    StringKeepDeepEquality,
    (contents) => contents.parsed,
    ParsedTextFileKeepDeepEquality,
    (contents) => contents.revisionsState,
    createCallWithTripleEquals<RevisionsStateType>(),
    textFileContents,
  )

export const TextFileKeepDeepEquality: KeepDeepEqualityCall<TextFile> = combine4EqualityCalls(
  (file) => file.fileContents,
  TextFileContentsKeepDeepEquality,
  (file) => file.lastSavedContents,
  nullableDeepEquality(TextFileContentsKeepDeepEquality),
  (file) => file.lastParseSuccess,
  nullableDeepEquality(ParseSuccessKeepDeepEquality),
  (file) => file.versionNumber,
  NumberKeepDeepEquality,
  textFile,
)

export const ImageFileKeepDeepEquality: KeepDeepEqualityCall<ImageFile> = combine6EqualityCalls(
  (file) => file.imageType,
  undefinableDeepEquality(StringKeepDeepEquality),
  (file) => file.base64,
  undefinableDeepEquality(StringKeepDeepEquality),
  (file) => file.width,
  undefinableDeepEquality(NumberKeepDeepEquality),
  (file) => file.height,
  undefinableDeepEquality(NumberKeepDeepEquality),
  (file) => file.hash,
  NumberKeepDeepEquality,
  (file) => file.gitBlobSha,
  undefinableDeepEquality(StringKeepDeepEquality),
  imageFile,
)

export const AssetFileKeepDeepEquality: KeepDeepEqualityCall<AssetFile> = combine2EqualityCalls(
  (file) => file.base64,
  undefinableDeepEquality(StringKeepDeepEquality),
  (file) => file.gitBlobSha,
  undefinableDeepEquality(StringKeepDeepEquality),
  assetFile,
)

export const TextOrImageOrAssetKeepDeepEquality: KeepDeepEqualityCall<
  TextFile | ImageFile | AssetFile
> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'TEXT_FILE':
      if (newValue.type === oldValue.type) {
        return TextFileKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'IMAGE_FILE':
      if (newValue.type === oldValue.type) {
        return ImageFileKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'ASSET_FILE':
      if (newValue.type === oldValue.type) {
        return AssetFileKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ProjectFileKeepDeepEquality: KeepDeepEqualityCall<ProjectFile> = (
  oldValue,
  newValue,
) => {
  if (newValue.type === 'DIRECTORY') {
    if (oldValue.type === 'DIRECTORY') {
      return DirectoryKeepDeepEquality(oldValue, newValue)
    } else {
      return keepDeepEqualityResult(newValue, false)
    }
  } else {
    if (oldValue.type === 'DIRECTORY') {
      return keepDeepEqualityResult(newValue, false)
    } else {
      return TextOrImageOrAssetKeepDeepEquality(oldValue, newValue)
    }
  }
}

export const ProjectContentDirectoryKeepDeepEquality: KeepDeepEqualityCall<ProjectContentDirectory> =
  combine3EqualityCalls(
    (dir) => dir.fullPath,
    StringKeepDeepEquality,
    (dir) => dir.directory,
    DirectoryKeepDeepEquality,
    (dir) => dir.children,
    ProjectContentTreeRootKeepDeepEquality(),
    projectContentDirectory,
  )

export const ProjectContentFileKeepDeepEquality: KeepDeepEqualityCall<ProjectContentFile> =
  combine2EqualityCalls(
    (file) => file.fullPath,
    StringKeepDeepEquality,
    (file) => file.content,
    TextOrImageOrAssetKeepDeepEquality,
    projectContentFile,
  )

export function ProjectContentsTreeKeepDeepEquality(): KeepDeepEqualityCall<ProjectContentsTree> {
  return (oldValue, newValue) => {
    switch (oldValue.type) {
      case 'PROJECT_CONTENT_DIRECTORY':
        if (newValue.type === oldValue.type) {
          return ProjectContentDirectoryKeepDeepEquality(oldValue, newValue)
        }
        break
      case 'PROJECT_CONTENT_FILE':
        if (newValue.type === oldValue.type) {
          return ProjectContentFileKeepDeepEquality(oldValue, newValue)
        }
        break
      default:
        const _exhaustiveCheck: never = oldValue
        throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
    }
    return keepDeepEqualityResult(newValue, false)
  }
}

export function ProjectContentTreeRootKeepDeepEquality(): KeepDeepEqualityCall<ProjectContentTreeRoot> {
  return objectDeepEquality(ProjectContentsTreeKeepDeepEquality())
}

export const FileChecksumsKeepDeepEquality: KeepDeepEqualityCall<FileChecksums | null> =
  nullableDeepEquality(objectDeepEquality(StringKeepDeepEquality))

export const FileWithChecksumKeepDeepEquality: KeepDeepEqualityCall<FileWithChecksum> =
  combine2EqualityCalls(
    (value) => value.file,
    ProjectFileKeepDeepEquality,
    (value) => value.checksum,
    StringKeepDeepEquality,
    (file, checksum) => {
      return {
        file: file,
        checksum: checksum,
      }
    },
  )

export const FileChecksumsWithFileKeepDeepEquality: KeepDeepEqualityCall<FileChecksumsWithFile> =
  objectDeepEquality(FileWithChecksumKeepDeepEquality)

export const DetailedTypeInfoMemberInfoKeepDeepEquality: KeepDeepEqualityCall<DetailedTypeInfoMemberInfo> =
  combine2EqualityCalls(
    (info) => info.type,
    StringKeepDeepEquality,
    (info) => info.members,
    objectDeepEquality(StringKeepDeepEquality),
    detailedTypeInfoMemberInfo,
  )

export const DetailedTypeInfoKeepDeepEquality: KeepDeepEqualityCall<DetailedTypeInfo> =
  combine2EqualityCalls(
    (info) => info.name,
    StringKeepDeepEquality,
    (info) => info.memberInfo,
    DetailedTypeInfoMemberInfoKeepDeepEquality,
    detailedTypeInfo,
  )

export const ExportTypeKeepDeepEquality: KeepDeepEqualityCall<ExportType> = combine3EqualityCalls(
  (expType) => expType.type,
  StringKeepDeepEquality,
  (expType) => expType.functionInfo,
  nullableDeepEquality(arrayDeepEquality(DetailedTypeInfoKeepDeepEquality)),
  (expType) => expType.reactClassInfo,
  nullableDeepEquality(DetailedTypeInfoKeepDeepEquality),
  exportType,
)

export const CodeResultKeepDeepEquality: KeepDeepEqualityCall<CodeResult> = combine3EqualityCalls(
  (result) => result.exports,
  objectDeepEquality(ExportTypeKeepDeepEquality),
  (result) => result.transpiledCode,
  NullableStringKeepDeepEquality,
  (result) => result.sourceMap,
  nullableDeepEquality(RawSourceMapKeepDeepEquality),
  codeResult,
)

export const ExportsInfoKeepDeepEquality: KeepDeepEqualityCall<ExportsInfo> = combine3EqualityCalls(
  (info) => info.filename,
  StringKeepDeepEquality,
  (info) => info.code,
  StringKeepDeepEquality,
  (info) => info.exportTypes,
  objectDeepEquality(ExportTypeKeepDeepEquality),
  exportsInfo,
)

export function ErrorKeepDeepEquality(
  oldValue: Error,
  newValue: Error,
): KeepDeepEqualityResult<Error> {
  return getIntrospectiveKeepDeepResult<Error>(oldValue, newValue)
}

export function FileEvaluationCacheExportsKeepDeepEquality(
  oldValue: any,
  newValue: any,
): KeepDeepEqualityResult<any> {
  return getIntrospectiveKeepDeepResult<any>(oldValue, newValue)
}

export const FileEvaluationCacheKeepDeepEquality: KeepDeepEqualityCall<FileEvaluationCache> =
  combine1EqualityCall(
    (cache) => cache.exports,
    FileEvaluationCacheExportsKeepDeepEquality,
    fileEvaluationCache,
  )

export const EvaluationCacheForPathKeepDeepEquality: KeepDeepEqualityCall<EvaluationCacheForPath> =
  combine2EqualityCalls(
    (cache) => cache.module,
    FileEvaluationCacheKeepDeepEquality,
    (cache) => cache.lastEvaluatedContent,
    StringKeepDeepEquality,
    evaluationCacheForPath,
  )

export const EvaluationCacheKeepDeepEquality: KeepDeepEqualityCall<EvaluationCache> =
  objectDeepEquality(EvaluationCacheForPathKeepDeepEquality)

export const CodeResultCacheKeepDeepEquality: KeepDeepEqualityCall<CodeResultCache> =
  combine7EqualityCalls(
    (cache) => cache.cache,
    objectDeepEquality(CodeResultKeepDeepEquality),
    (cache) => cache.exportsInfo,
    arrayDeepEquality(ExportsInfoKeepDeepEquality),
    (cache) => cache.error,
    nullableDeepEquality(ErrorKeepDeepEquality),
    (cache) => cache.curriedRequireFn,
    createCallWithTripleEquals<CurriedUtopiaRequireFn>(),
    (cache) => cache.curriedResolveFn,
    createCallWithTripleEquals<CurriedResolveFn>(),
    (cache) => cache.projectModules,
    MultiFileBuildResultKeepDeepEquality,
    (cache) => cache.evaluationCache,
    EvaluationCacheKeepDeepEquality,
    codeResultCache,
  )

export const ComponentElementToInsertKeepDeepEquality: KeepDeepEqualityCall<ComponentElementToInsert> =
  unionDeepEquality(
    createCallWithTripleEquals<ComponentElementToInsert>(),
    JSXElementWithoutUIDKeepDeepEquality(),
    (p): p is JSXConditionalExpressionWithoutUID => p.type === 'JSX_CONDITIONAL_EXPRESSION',
    (p): p is JSXElementWithoutUID => p.type === 'JSX_ELEMENT',
  )

export const ComponentInfoKeepDeepEquality: KeepDeepEqualityCall<ComponentInfo> =
  combine3EqualityCalls(
    (info) => info.insertMenuLabel,
    StringKeepDeepEquality,
    (info) => info.elementToInsert,
    createCallWithTripleEquals<() => ComponentElementToInsert>(),
    (info) => info.importsToAdd,
    objectDeepEquality(ImportDetailsKeepDeepEquality),
    componentInfo,
  )

export function PropertyControlsKeepDeepEquality(
  oldValue: PropertyControls,
  newValue: PropertyControls,
): KeepDeepEqualityResult<PropertyControls> {
  return getIntrospectiveKeepDeepResult<PropertyControls>(oldValue, newValue) // Do these lazily for now.
}

const PreferredChildComponentDescriptorKeepDeepEquality: KeepDeepEqualityCall<PreferredChildComponentDescriptor> =
  combine3EqualityCalls(
    (d) => d.name,
    StringKeepDeepEquality,
    (d) => d.moduleName,
    nullableDeepEquality(StringKeepDeepEquality),
    (d) => d.variants,
    arrayDeepEquality(createCallWithTripleEquals()),
    (name, moduleName, variants) => ({ name, moduleName, variants }),
  )

export const DescriptorFileComponentDescriptorKeepDeepEquality: KeepDeepEqualityCall<ComponentDescriptorFromDescriptorFile> =
  combine3EqualityCalls(
    (descriptor) => descriptor.type,
    StringKeepDeepEquality,
    (descriptor) => descriptor.sourceDescriptorFile,
    StringKeepDeepEquality,
    (descriptor) => descriptor.bounds,
    nullableDeepEquality(ComponentDescriptorBoundsKeepDeepEquality),
    (_, sourceDescriptorFile, lineNumber) =>
      componentDescriptorFromDescriptorFile(sourceDescriptorFile, lineNumber),
  )

export function ComponentDescriptorSourceKeepDeepEquality(): KeepDeepEqualityCall<ComponentDescriptorSource> {
  return (oldValue, newValue) => {
    switch (oldValue.type) {
      case 'DEFAULT':
        if (newValue.type === oldValue.type) {
          return keepDeepEqualityResult(oldValue, true)
        }
        break
      case 'DESCRIPTOR_FILE':
        if (newValue.type === oldValue.type) {
          return DescriptorFileComponentDescriptorKeepDeepEquality(oldValue, newValue)
        }
        break
      default:
        const _exhaustiveCheck: never = oldValue
        throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
    }
    return keepDeepEqualityResult(newValue, false)
  }
}

const InspectorSpecKeepDeepEquality: KeepDeepEqualityCall<TypedInpsectorSpec> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'hidden':
      if (oldValue.type === newValue.type) {
        return keepDeepEqualityResult(oldValue, true)
      }
      break
    case 'shown':
      if (oldValue.type === newValue.type) {
        return combine2EqualityCalls<StyleSectionState, Styling[], ShownInspectorSpec>(
          (i) => i.display,
          createCallWithTripleEquals(),
          (i) => i.sections,
          arrayDeepEquality(createCallWithTripleEquals()),
          (display, sections) => ({ type: 'shown', display: display, sections: sections }),
        )(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ComponentDescriptorKeepDeepEquality: KeepDeepEqualityCall<ComponentDescriptor> =
  combine10EqualityCalls(
    (descriptor) => descriptor.properties,
    PropertyControlsKeepDeepEquality,
    (descriptor) => descriptor.supportsChildren,
    BooleanKeepDeepEquality,
    (descriptor) => descriptor.variants,
    arrayDeepEquality(ComponentInfoKeepDeepEquality),
    (descriptor) => descriptor.preferredChildComponents,
    arrayDeepEquality(PreferredChildComponentDescriptorKeepDeepEquality),
    (descriptor) => descriptor.source,
    ComponentDescriptorSourceKeepDeepEquality(),
    (descriptor) => descriptor.focus,
    createCallWithTripleEquals<Focus>(),
    (descriptor) => descriptor.inspector,
    InspectorSpecKeepDeepEquality,
    (descriptor) => descriptor.emphasis,
    createCallWithTripleEquals<Emphasis>(),
    (descriptor) => descriptor.icon,
    createCallWithTripleEquals<Icon>(),
    (descriptor) => descriptor.label,
    nullableDeepEquality(StringKeepDeepEquality),
    componentDescriptor,
  )

export const ComponentDescriptorsForFileKeepDeepEquality: KeepDeepEqualityCall<ComponentDescriptorsForFile> =
  objectDeepEquality(ComponentDescriptorKeepDeepEquality)

export const PropertyControlsInfoKeepDeepEquality: KeepDeepEqualityCall<PropertyControlsInfo> =
  objectDeepEquality(ComponentDescriptorsForFileKeepDeepEquality)

export const TargetedInsertionParentKeepDeepEquality: KeepDeepEqualityCall<TargetedInsertionParent> =
  combine2EqualityCalls(
    (parent) => parent.target,
    ElementPathKeepDeepEquality,
    (parent) => parent.staticTarget,
    StaticElementPathKeepDeepEquality,
    targetedInsertionParent,
  )

export const SizeKeepDeepEquality: KeepDeepEqualityCall<Size> = combine2EqualityCalls(
  (s) => s.width,
  NumberKeepDeepEquality,
  (s) => s.height,
  NumberKeepDeepEquality,
  size,
)

export const InsertionSubjectWrapperKeepDeepEquality: KeepDeepEqualityCall<InsertionSubjectWrapper> =
  createCallWithTripleEquals()

export const ChildInsertionPathKeepDeepEquality: KeepDeepEqualityCall<ChildInsertionPath> =
  combine1EqualityCall(
    (c) => c.intendedParentPath,
    StaticElementPathKeepDeepEquality,
    childInsertionPath,
  )

export const ConditionalCaseKeepDeepEquality: KeepDeepEqualityCall<ConditionalCase> =
  createCallWithTripleEquals<ConditionalCase>()

export const ConditionalClauseInsertBehaviorKeepDeepEquality: KeepDeepEqualityCall<ConditionalClauseInsertBehavior> =
  createCallWithTripleEquals<ConditionalClauseInsertBehavior>()

export const ConditionalClauseInsertionPathKeepDeepEquality: KeepDeepEqualityCall<ConditionalClauseInsertionPath> =
  combine3EqualityCalls(
    (c) => c.intendedParentPath,
    StaticElementPathKeepDeepEquality,
    (c) => c.clause,
    ConditionalCaseKeepDeepEquality,
    (c) => c.insertBehavior,
    ConditionalClauseInsertBehaviorKeepDeepEquality,
    conditionalClauseInsertionPath,
  )

export function InsertionPathKeepDeepEquality(): KeepDeepEqualityCall<InsertionPath> {
  return (oldValue, newValue) => {
    switch (oldValue.type) {
      case 'CHILD_INSERTION':
        if (newValue.type === oldValue.type) {
          return ChildInsertionPathKeepDeepEquality(oldValue, newValue)
        }
        break
      case 'CONDITIONAL_CLAUSE_INSERTION':
        if (newValue.type === oldValue.type) {
          return ConditionalClauseInsertionPathKeepDeepEquality(oldValue, newValue)
        }
        break
      default:
        assertNever(oldValue)
    }
    return keepDeepEqualityResult(newValue, false)
  }
}

export const InsertionSubjectKeepDeepEquality: KeepDeepEqualityCall<InsertionSubject> =
  combine7EqualityCalls(
    (subject) => subject.uid,
    StringKeepDeepEquality,
    (subject) => subject.element,
    JSXElementKeepDeepEquality,
    (subject) => subject.defaultSize,
    nullableDeepEquality(SizeKeepDeepEquality),
    (subject) => subject.importsToAdd,
    objectDeepEquality(ImportDetailsKeepDeepEquality),
    (subject) => subject.parent,
    nullableDeepEquality(TargetedInsertionParentKeepDeepEquality),
    (subject) => subject.textEdit,
    BooleanKeepDeepEquality,
    (subject) => subject.insertionSubjectWrapper,
    nullableDeepEquality(InsertionSubjectWrapperKeepDeepEquality),
    insertionSubject,
  )

export const ImageInsertionSubjectKeepDeepEquality: KeepDeepEqualityCall<ImageInsertionSubject> =
  combine2EqualityCalls(
    (s) => s.file,
    ImageFileKeepDeepEquality,
    (s) => s.path,
    StringKeepDeepEquality,
    imageInsertionSubject,
  )

export const InsertModeKeepDeepEquality: KeepDeepEqualityCall<InsertMode> = combine1EqualityCall(
  (mode) => mode.subjects,
  arrayDeepEquality(InsertionSubjectKeepDeepEquality),
  EditorModes.insertMode,
)

export const SelectModeKeepDeepEquality: KeepDeepEqualityCall<SelectMode> = combine3EqualityCalls(
  (mode) => mode.controlId,
  NullableStringKeepDeepEquality,
  (mode) => mode.area,
  BooleanKeepDeepEquality,
  (mode) => mode.toolbarMode,
  createCallWithTripleEquals<SelectModeToolbarMode>(),
  EditorModes.selectMode,
)

export const LiveCanvasModeKeepDeepEquality: KeepDeepEqualityCall<LiveCanvasMode> =
  combine1EqualityCall(
    (mode) => mode.controlId,
    NullableStringKeepDeepEquality,
    EditorModes.liveMode,
  )

export const CoordinateKeepDeepEquality: KeepDeepEqualityCall<Coordinates> = combine2EqualityCalls(
  (c) => c.x,
  NumberKeepDeepEquality,
  (c) => c.y,
  NumberKeepDeepEquality,
  (x: number, y: number) => ({ x, y }),
)

export const TextEditableElementStateKeepDeepEquality: KeepDeepEqualityCall<
  TextEditableElementState
> = (oldValue, newValue) => {
  if (oldValue === newValue) {
    return keepDeepEqualityResult(newValue, true)
  }
  return keepDeepEqualityResult(oldValue, false)
}

export const TextEditModeKeepDeepEquality: KeepDeepEqualityCall<TextEditMode> =
  combine4EqualityCalls(
    (mode) => mode.editedText,
    ElementPathKeepDeepEquality,
    (mode) => mode.cursorPosition,
    nullableDeepEquality(CoordinateKeepDeepEquality),
    (mode) => mode.elementState,
    TextEditableElementStateKeepDeepEquality,
    (mode) => mode.selectOnFocus,
    createCallWithTripleEquals<'select-all-on-focus' | 'no-text-selection'>(),
    EditorModes.textEditMode,
  )

export const CanvasCommentLocationKeepDeepEquality: KeepDeepEqualityCall<CanvasCommentLocation> =
  combine1EqualityCall((loc) => loc.position, CanvasPointKeepDeepEquality, canvasCommentLocation)

export const SceneCommentLocationKeepDeepEquality: KeepDeepEqualityCall<SceneCommentLocation> =
  combine3EqualityCalls(
    (loc) => loc.sceneId,
    StringKeepDeepEquality,
    (loc) => loc.offset,
    LocalPointKeepDeepEquality,
    (loc) => loc.position,
    CanvasPointKeepDeepEquality,
    sceneCommentLocation,
  )

export const NewCommentLocationKeepDeepEquality: KeepDeepEqualityCall<NewCommentLocation> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'canvas':
      if (newValue.type === oldValue.type) {
        return CanvasCommentLocationKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'scene':
      if (newValue.type === oldValue.type) {
        return SceneCommentLocationKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const NewCommentKeepDeepEquality: KeepDeepEqualityCall<NewComment> = combine1EqualityCall(
  (mode) => mode.location,
  NewCommentLocationKeepDeepEquality,
  newComment,
)

export const ExistingCommentKeepDeepEquality: KeepDeepEqualityCall<ExistingComment> =
  combine1EqualityCall((mode) => mode.threadId, StringKeepDeepEquality, existingComment)

export const CommentIdKeepDeepEquality: KeepDeepEqualityCall<CommentId> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'new':
      if (newValue.type === oldValue.type) {
        return NewCommentKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'existing':
      if (newValue.type === oldValue.type) {
        return ExistingCommentKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const CommentModeKeepDeepEquality: KeepDeepEqualityCall<CommentMode> = combine2EqualityCalls(
  (mode) => mode.comment,
  nullableDeepEquality(CommentIdKeepDeepEquality),
  (mode) => mode.isDragging,
  createCallWithTripleEquals<IsDragging>(),
  EditorModes.commentMode,
)

export const FollowModeKeepDeepEquality: KeepDeepEqualityCall<FollowMode> = combine2EqualityCalls(
  (mode) => mode.playerId,
  StringKeepDeepEquality,
  (mode) => mode.connectionId,
  NumberKeepDeepEquality,
  EditorModes.followMode,
)

export const ModeKeepDeepEquality: KeepDeepEqualityCall<Mode> = (oldValue, newValue) => {
  switch (oldValue.type) {
    case 'insert':
      if (newValue.type === oldValue.type) {
        return InsertModeKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'select':
      if (newValue.type === oldValue.type) {
        return SelectModeKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'live':
      if (newValue.type === oldValue.type) {
        return LiveCanvasModeKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'textEdit':
      if (newValue.type === oldValue.type) {
        return TextEditModeKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'comment':
      if (newValue.type === oldValue.type) {
        return CommentModeKeepDeepEquality(newValue, oldValue)
      }
      break
    case 'follow':
      if (newValue.type === oldValue.type) {
        return FollowModeKeepDeepEquality(newValue, oldValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const AllElementPropsKeepDeepEquality: KeepDeepEqualityCall<AllElementProps> =
  objectDeepEquality(objectDeepEquality(createCallFromIntrospectiveKeepDeep()))

export const NoticeKeepDeepEquality: KeepDeepEqualityCall<Notice> = combine4EqualityCalls(
  (note) => note.message,
  createCallWithTripleEquals<React.ReactChild>(),
  (note) => note.level,
  createCallWithTripleEquals<NoticeLevel>(),
  (note) => note.persistent,
  BooleanKeepDeepEquality,
  (note) => note.id,
  StringKeepDeepEquality,
  notice,
)

export const CursorStackItemKeepDeepEquality: KeepDeepEqualityCall<CursorStackItem> =
  combine3EqualityCalls(
    (item) => item.id,
    StringKeepDeepEquality,
    (item) => item.importance,
    createCallWithTripleEquals<CursorImportanceLevel>(),
    (item) => item.cursor,
    createCallWithTripleEquals<CSSCursor>(),
    cursorStackItem,
  )

export const CanvasCursorKeepDeepEquality: KeepDeepEqualityCall<CanvasCursor> =
  combine2EqualityCalls(
    (cursor) => cursor.fixed,
    nullableDeepEquality(CursorStackItemKeepDeepEquality),
    (cursor) => cursor.mouseOver,
    arrayDeepEquality(CursorStackItemKeepDeepEquality),
    canvasCursor,
  )

// Here to cause the build to break if `Front` is changed.
front()
export const FrontKeepDeepEquality: KeepDeepEqualityCall<Front> = (oldValue, newValue) => {
  return keepDeepEqualityResult(oldValue, true)
}

// Here to cause the build to break if `Back` is changed.
back()
export const BackKeepDeepEquality: KeepDeepEqualityCall<Back> = (oldValue, newValue) => {
  return keepDeepEqualityResult(oldValue, true)
}

export const AbsoluteKeepDeepEquality: KeepDeepEqualityCall<Absolute> = combine1EqualityCall(
  (abs) => abs.index,
  NumberKeepDeepEquality,
  absolute,
)

export const AfterKeepDeepEquality: KeepDeepEqualityCall<After> = combine1EqualityCall(
  (aft) => aft.index,
  NumberKeepDeepEquality,
  after,
)

export const BeforeKeepDeepEquality: KeepDeepEqualityCall<Before> = combine1EqualityCall(
  (bef) => bef.index,
  NumberKeepDeepEquality,
  before,
)

export function IndexPositionKeepDeepEquality(
  oldValue: IndexPosition,
  newValue: IndexPosition,
): KeepDeepEqualityResult<IndexPosition> {
  switch (oldValue.type) {
    case 'front':
      if (newValue.type === oldValue.type) {
        return FrontKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'back':
      if (newValue.type === oldValue.type) {
        return BackKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'absolute':
      if (newValue.type === oldValue.type) {
        return AbsoluteKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'after':
      if (newValue.type === oldValue.type) {
        return AfterKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'before':
      if (newValue.type === oldValue.type) {
        return BeforeKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const EditorStateInspectorKeepDeepEquality: KeepDeepEqualityCall<EditorStateInspector> =
  combine2EqualityCalls(
    (inspector) => inspector.visible,
    BooleanKeepDeepEquality,
    (inspector) => inspector.classnameFocusCounter,
    NumberKeepDeepEquality,
    editorStateInspector,
  )

export const DraggedImagePropertiesDeepEquality: KeepDeepEqualityCall<DraggedImageProperties> =
  combine3EqualityCalls(
    (draggedImage) => draggedImage.width,
    NumberKeepDeepEquality,
    (draggedImage) => draggedImage.height,
    NumberKeepDeepEquality,
    (draggedImage) => draggedImage.src,
    StringKeepDeepEquality,
    draggedImageProperties,
  )

export const DraggingFromSidebarKeepDeepEquality: KeepDeepEqualityCall<DraggingFromSidebar> =
  combine2EqualityCalls(
    (d) => d.draggedImageProperties,
    nullableDeepEquality(DraggedImagePropertiesDeepEquality),
    (d) => d.type,
    StringKeepDeepEquality,
    draggingFromSidebar,
  )

export const DragSessionStateKeepDeepEquality: KeepDeepEqualityCall<ImageDragSessionState> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'NOT_DRAGGING':
      if (newValue.type === oldValue.type) {
        return keepDeepEqualityResult(oldValue, true)
      }
      break
    case 'DRAGGING_FROM_FS':
      if (newValue.type === oldValue.type) {
        return keepDeepEqualityResult(oldValue, true)
      }
      break
    case 'DRAGGING_FROM_SIDEBAR':
      if (newValue.type === oldValue.type) {
        return DraggingFromSidebarKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const EditorStateFileBrowserKeepDeepEquality: KeepDeepEqualityCall<EditorStateFileBrowser> =
  combine3EqualityCalls(
    (fileBrowser) => fileBrowser.minimised,
    BooleanKeepDeepEquality,
    (fileBrowser) => fileBrowser.renamingTarget,
    NullableStringKeepDeepEquality,
    (fileBrowser) => fileBrowser.dropTarget,
    NullableStringKeepDeepEquality,
    editorStateFileBrowser,
  )

export const EditorStateDependencyListKeepDeepEquality: KeepDeepEqualityCall<EditorStateDependencyList> =
  combine1EqualityCall(
    (depList) => depList.minimised,
    BooleanKeepDeepEquality,
    editorStateDependencyList,
  )

export const EditorStateGenericExternalResourcesKeepDeepEquality: KeepDeepEqualityCall<EditorStateGenericExternalResources> =
  combine1EqualityCall(
    (resources) => resources.minimised,
    BooleanKeepDeepEquality,
    editorStateGenericExternalResources,
  )

export const EditorStateGoogleFontsResourcesKeepDeepEquality: KeepDeepEqualityCall<EditorStateGoogleFontsResources> =
  combine1EqualityCall(
    (resources) => resources.minimised,
    BooleanKeepDeepEquality,
    editorStateGoogleFontsResources,
  )

export const EditorStateProjectSettingsKeepDeepEquality: KeepDeepEqualityCall<EditorStateProjectSettings> =
  combine1EqualityCall(
    (settings) => settings.minimised,
    BooleanKeepDeepEquality,
    editorStateProjectSettings,
  )

export const EditorStateTopMenuKeepDeepEquality: KeepDeepEqualityCall<EditorStateTopMenu> =
  combine2EqualityCalls(
    (menu) => menu.formulaBarMode,
    createCallWithTripleEquals<'css' | 'content'>(),
    (menu) => menu.formulaBarFocusCounter,
    NumberKeepDeepEquality,
    editorStateTopMenu,
  )

export const EditorStatePreviewKeepDeepEquality: KeepDeepEqualityCall<EditorStatePreview> =
  combine2EqualityCalls(
    (preview) => preview.visible,
    BooleanKeepDeepEquality,
    (preview) => preview.connected,
    BooleanKeepDeepEquality,
    editorStatePreview,
  )

export const EditorStateHomeKeepDeepEquality: KeepDeepEqualityCall<EditorStateHome> =
  combine1EqualityCall((preview) => preview.visible, BooleanKeepDeepEquality, editorStateHome)

export const EditorStateLockedElementsDeepEquality: KeepDeepEqualityCall<LockedElements> =
  combine2EqualityCalls(
    (locked) => locked.simpleLock,
    ElementPathArrayKeepDeepEquality,
    (locked) => locked.hierarchyLock,
    ElementPathArrayKeepDeepEquality,
    (simpleLock: Array<ElementPath>, hierarchyLock: Array<ElementPath>) => ({
      simpleLock: simpleLock,
      hierarchyLock: hierarchyLock,
    }),
  )

export function CSSColorKeepDeepEquality(
  oldValue: CSSColor,
  newValue: CSSColor,
): KeepDeepEqualityResult<CSSColor> {
  return getIntrospectiveKeepDeepResult<CSSColor>(oldValue, newValue)
}

export function CSSFontFamilyKeepDeepEquality(
  oldValue: CSSFontFamily,
  newValue: CSSFontFamily,
): KeepDeepEqualityResult<CSSFontFamily> {
  return getIntrospectiveKeepDeepResult<CSSFontFamily>(oldValue, newValue)
}

export function CSSFontWeightAndStyleKeepDeepEquality(
  oldValue: CSSFontWeightAndStyle,
  newValue: CSSFontWeightAndStyle,
): KeepDeepEqualityResult<CSSFontWeightAndStyle> {
  return getIntrospectiveKeepDeepResult<CSSFontWeightAndStyle>(oldValue, newValue)
}

export function CSSFontSizeKeepDeepEquality(
  oldValue: CSSFontSize,
  newValue: CSSFontSize,
): KeepDeepEqualityResult<CSSFontSize> {
  return getIntrospectiveKeepDeepResult<CSSFontSize>(oldValue, newValue)
}

export function CSSTextAlignKeepDeepEquality(
  oldValue: CSSTextAlign,
  newValue: CSSTextAlign,
): KeepDeepEqualityResult<CSSTextAlign> {
  return getIntrospectiveKeepDeepResult<CSSTextAlign>(oldValue, newValue)
}

export function CSSTextDecorationLineKeepDeepEquality(
  oldValue: CSSTextDecorationLine,
  newValue: CSSTextDecorationLine,
): KeepDeepEqualityResult<CSSTextDecorationLine> {
  return getIntrospectiveKeepDeepResult<CSSTextDecorationLine>(oldValue, newValue)
}

export function CSSLetterSpacingKeepDeepEquality(
  oldValue: CSSLetterSpacing,
  newValue: CSSLetterSpacing,
): KeepDeepEqualityResult<CSSLetterSpacing> {
  return getIntrospectiveKeepDeepResult<CSSLetterSpacing>(oldValue, newValue)
}

export function CSSLineHeightKeepDeepEquality(
  oldValue: CSSLineHeight,
  newValue: CSSLineHeight,
): KeepDeepEqualityResult<CSSLineHeight> {
  return getIntrospectiveKeepDeepResult<CSSLineHeight>(oldValue, newValue)
}

export const FontSettingsKeepDeepEquality: KeepDeepEqualityCall<FontSettings> =
  combine8EqualityCalls(
    (settings) => settings.color,
    CSSColorKeepDeepEquality,
    (settings) => settings.fontFamily,
    CSSFontFamilyKeepDeepEquality,
    (settings) => settings.fontWeightAndStyle,
    CSSFontWeightAndStyleKeepDeepEquality,
    (settings) => settings.fontSize,
    CSSFontSizeKeepDeepEquality,
    (settings) => settings.textAlign,
    CSSTextAlignKeepDeepEquality,
    (settings) => settings.textDecorationLine,
    CSSTextDecorationLineKeepDeepEquality,
    (settings) => settings.letterSpacing,
    CSSLetterSpacingKeepDeepEquality,
    (settings) => settings.lineHeight,
    CSSLineHeightKeepDeepEquality,
    fontSettings,
  )

export const FileDeleteModalKeepDeepEquality: KeepDeepEqualityCall<FileDeleteModal> =
  combine1EqualityCall((modal) => modal.filePath, StringKeepDeepEquality, fileDeleteModal)

export const AssetResultKeepDeepEquality: KeepDeepEqualityCall<AssetResult> = combine4EqualityCalls(
  (result) => result.filename,
  StringKeepDeepEquality,
  (result) => result.base64Bytes,
  StringKeepDeepEquality,
  (result) => result.hash,
  NumberKeepDeepEquality,
  (result) => result.gitBlobSha,
  StringKeepDeepEquality,
  assetResult,
)

export const ImageResultKeepDeepEquality: KeepDeepEqualityCall<ImageResult> = combine6EqualityCalls(
  (result) => result.filename,
  StringKeepDeepEquality,
  (result) => result.base64Bytes,
  StringKeepDeepEquality,
  (result) => result.size,
  SizeKeepDeepEquality,
  (result) => result.fileType,
  StringKeepDeepEquality,
  (result) => result.hash,
  NumberKeepDeepEquality,
  (result) => result.gitBlobSha,
  StringKeepDeepEquality,
  imageResult,
)

export const TextResultKeepDeepEquality: KeepDeepEqualityCall<TextResult> = combine2EqualityCalls(
  (result) => result.filename,
  StringKeepDeepEquality,
  (result) => result.content,
  StringKeepDeepEquality,
  textResult,
)

export const FileResultKeepDeepEquality: KeepDeepEqualityCall<FileResult> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'ASSET_RESULT':
      if (newValue.type === oldValue.type) {
        return AssetResultKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'IMAGE_RESULT':
      if (newValue.type === oldValue.type) {
        return ImageResultKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'TEXT_RESULT':
      if (newValue.type === oldValue.type) {
        return TextResultKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const FileUploadInfoKeepDeepEquality: KeepDeepEqualityCall<FileUploadInfo> =
  combine2EqualityCalls(
    (file) => file.fileResult,
    FileResultKeepDeepEquality,
    (file) => file.targetPath,
    StringKeepDeepEquality,
    fileUploadInfo,
  )

export const FileOverwriteModalKeepDeepEquality: KeepDeepEqualityCall<FileOverwriteModal> =
  combine1EqualityCall(
    (modal) => modal.files,
    arrayDeepEquality(FileUploadInfoKeepDeepEquality),
    fileOverwriteModal,
  )

export const FileRevertModalKeepDeepEquality: KeepDeepEqualityCall<FileRevertModal> =
  combine2EqualityCalls(
    (modal) => modal.filePath,
    StringKeepDeepEquality,
    (modal) => modal.status,
    nullableDeepEquality(GithubFileStatusKeepDeepEquality),
    fileRevertModal,
  )

export const ModalDialogKeepDeepEquality: KeepDeepEqualityCall<ModalDialog> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'file-delete':
      if (newValue.type === oldValue.type) {
        return FileDeleteModalKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'file-overwrite':
      if (newValue.type === oldValue.type) {
        return FileOverwriteModalKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'file-revert':
      if (newValue.type === oldValue.type) {
        return FileRevertModalKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'file-revert-all':
      if (newValue.type === oldValue.type) {
        return keepDeepEqualityResult(oldValue, true)
      } else {
        return keepDeepEqualityResult(newValue, false)
      }
    case 'disconnect-github-project':
      if (newValue.type === oldValue.type) {
        return keepDeepEqualityResult(oldValue, true)
      } else {
        return keepDeepEqualityResult(newValue, false)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ProjectListingKeepDeepEquality: KeepDeepEqualityCall<ProjectListing> =
  combine5EqualityCalls(
    (listing) => listing.id,
    StringKeepDeepEquality,
    (listing) => listing.title,
    StringKeepDeepEquality,
    (listing) => listing.createdAt,
    StringKeepDeepEquality,
    (listing) => listing.modifiedAt,
    StringKeepDeepEquality,
    (listing) => listing.thumbnail,
    StringKeepDeepEquality,
    projectListing,
  )

export const ErrorMessagesKeepDeepEquality: KeepDeepEqualityCall<ErrorMessages> =
  objectDeepEquality(arrayDeepEquality(ErrorMessageKeepDeepEquality))

export const EditorStateCodeEditorErrorsKeepDeepEquality: KeepDeepEqualityCall<EditorStateCodeEditorErrors> =
  combine3EqualityCalls(
    (errors) => errors.buildErrors,
    ErrorMessagesKeepDeepEquality,
    (errors) => errors.lintErrors,
    ErrorMessagesKeepDeepEquality,
    (errors) => errors.componentDescriptorErrors,
    ErrorMessagesKeepDeepEquality,
    editorStateCodeEditorErrors,
  )

export const UtopiaVSCodeConfigKeepDeepEquality: KeepDeepEqualityCall<UtopiaVSCodeConfig> =
  combine1EqualityCall(
    (config) => config.followSelection,
    combine1EqualityCall(
      (follow) => follow.enabled,
      BooleanKeepDeepEquality,
      (enabled) => {
        return {
          enabled: enabled,
        }
      },
    ),
    (followSelection) => {
      return {
        followSelection: followSelection,
      }
    },
  )

export const GithubRepoKeepDeepEquality: KeepDeepEqualityCall<GithubRepo> = combine2EqualityCalls(
  (repo) => repo.owner,
  createCallWithTripleEquals(),
  (repo) => repo.repository,
  createCallWithTripleEquals(),
  githubRepo,
)

export const GithubBranchKeepDeepEquality: KeepDeepEqualityCall<GithubBranch> =
  combine1EqualityCall(
    (branch) => branch.name,
    StringKeepDeepEquality,
    (name: string): GithubBranch => ({ name }),
  )

export const RepositoryEntryPermissionsKeepDeepEquality: KeepDeepEqualityCall<RepositoryEntryPermissions> =
  combine3EqualityCalls(
    (p) => p.admin,
    BooleanKeepDeepEquality,
    (p) => p.pull,
    BooleanKeepDeepEquality,
    (p) => p.push,
    BooleanKeepDeepEquality,
    repositoryEntryPermissions,
  )

export const RepositoryEntryKeepDeepEquality: KeepDeepEqualityCall<RepositoryEntry> =
  combine8EqualityCalls(
    (r) => r.avatarUrl,
    NullableStringKeepDeepEquality,
    (r) => r.isPrivate,
    BooleanKeepDeepEquality,
    (r) => r.fullName,
    StringKeepDeepEquality,
    (r) => r.name,
    StringKeepDeepEquality,
    (r) => r.description,
    NullableStringKeepDeepEquality,
    (r) => r.updatedAt,
    NullableStringKeepDeepEquality,
    (r) => r.defaultBranch,
    StringKeepDeepEquality,
    (r) => r.permissions,
    RepositoryEntryPermissionsKeepDeepEquality,
    repositoryEntry,
  )

export const ProjectGithubSettingsKeepDeepEquality: KeepDeepEqualityCall<ProjectGithubSettings> =
  combine5EqualityCalls(
    (settings) => settings.targetRepository,
    nullableDeepEquality(GithubRepoKeepDeepEquality),
    (settings) => settings.originCommit,
    nullableDeepEquality(createCallWithTripleEquals<string>()),
    (settings) => settings.branchName,
    nullableDeepEquality(createCallWithTripleEquals<string>()),
    (settings) => settings.pendingCommit,
    nullableDeepEquality(createCallWithTripleEquals<string>()),
    (settings) => settings.branchLoaded,
    BooleanKeepDeepEquality,
    projectGithubSettings,
  )

export const GithubFileChangesKeepDeepEquality: KeepDeepEqualityCall<GithubFileChanges> =
  combine3EqualityCalls(
    (settings) => settings.modified,
    arrayDeepEquality(StringKeepDeepEquality),
    (settings) => settings.untracked,
    arrayDeepEquality(StringKeepDeepEquality),
    (settings) => settings.deleted,
    arrayDeepEquality(StringKeepDeepEquality),
    emptyGithubFileChanges,
  )

export const PullRequestKeepDeepEquality: KeepDeepEqualityCall<PullRequest> = combine3EqualityCalls(
  (data) => data.htmlURL,
  StringKeepDeepEquality,
  (data) => data.number,
  NumberKeepDeepEquality,
  (data) => data.title,
  StringKeepDeepEquality,
  (htmlURL, number, title) => ({ htmlURL: htmlURL, number: number, title: title }),
)

export const GithubUserKeepDeepEquality: KeepDeepEqualityCall<GithubUser> = combine4EqualityCalls(
  (data) => data.avatarURL,
  StringKeepDeepEquality,
  (data) => data.htmlURL,
  StringKeepDeepEquality,
  (data) => data.login,
  StringKeepDeepEquality,
  (data) => data.name,
  NullableStringKeepDeepEquality,
  (login, avatarURL, htmlURL, name) => ({
    login: login,
    avatarURL: avatarURL,
    htmlURL: htmlURL,
    name: name,
  }),
)

export const GithubDataKeepDeepEquality: KeepDeepEqualityCall<GithubData> = combine9EqualityCalls(
  (data) => data.branches,
  nullableDeepEquality(arrayDeepEquality(GithubBranchKeepDeepEquality)),
  (data) => data.userRepositories,
  arrayDeepEquality(RepositoryEntryKeepDeepEquality),
  (data) => data.publicRepositories,
  arrayDeepEquality(RepositoryEntryKeepDeepEquality),
  (data) => data.treeConflicts,
  createCallWithTripleEquals(),
  (data) => data.lastUpdatedAt,
  NullableNumberKeepDeepEquality,
  (data) => data.upstreamChanges,
  nullableDeepEquality(GithubFileChangesKeepDeepEquality),
  (data) => data.currentBranchPullRequests,
  nullableDeepEquality(arrayDeepEquality(PullRequestKeepDeepEquality)),
  (data) => data.githubUserDetails,
  nullableDeepEquality(GithubUserKeepDeepEquality),
  (data) => data.lastRefreshedCommit,
  NullableStringKeepDeepEquality,
  newGithubData,
)

export const GithubOperationKeepDeepEquality: KeepDeepEqualityCall<GithubOperation> = (
  oldValue,
  newValue,
) => {
  if (oldValue.name !== newValue.name) {
    return keepDeepEqualityResult(newValue, false)
  }
  return keepDeepEqualityResult(oldValue, true)
}

export const GithubOperationsKeepDeepEquality: KeepDeepEqualityCall<Array<GithubOperation>> =
  arrayDeepEquality(GithubOperationKeepDeepEquality)

export const ColorSwatchDeepEquality: KeepDeepEqualityCall<ColorSwatch> = combine2EqualityCalls(
  (c) => c.id,
  StringKeepDeepEquality,
  (c) => c.hex,
  StringKeepDeepEquality,
  newColorSwatch,
)

export const ValueAtPathDeepEquality: KeepDeepEqualityCall<ValueAtPath> = combine2EqualityCalls(
  (c) => c.path,
  PropertyPathKeepDeepEquality(),
  (c) => c.value,
  JSExpressionKeepDeepEqualityCall,
  valueAtPath,
)

export const JSXElementsCopyDataDeepEquality: KeepDeepEqualityCall<CopyData> =
  combine4EqualityCalls(
    (c) => c.copyDataWithPropsReplaced,
    nullableDeepEquality(ElementPasteWithMetadataKeepDeepEquality),
    (c) => c.copyDataWithPropsPreserved,
    ElementPasteWithMetadataKeepDeepEquality,
    (c) => c.targetOriginalContextElementPathTrees,
    ElementPathTreesKeepDeepEquality(),
    (c) => c.originalAllElementProps,
    AllElementPropsKeepDeepEquality,
    (
      copyDataWithPropsReplaced,
      copyDataWithPropsPreserved,
      targetOriginalContextElementPathTrees,
      originalAllElementProps,
    ) => ({
      copyDataWithPropsReplaced,
      copyDataWithPropsPreserved,
      targetOriginalContextElementPathTrees,
      originalAllElementProps,
    }),
  )

export const InternalClipboardKeepDeepEquality: KeepDeepEqualityCall<InternalClipboard> =
  combine2EqualityCalls(
    (data) => data.styleClipboard,
    arrayDeepEquality(ValueAtPathDeepEquality),
    (data) => data.elements,
    arrayDeepEquality(JSXElementsCopyDataDeepEquality),
    internalClipboard,
  )

export const PastePostActionMenuDataKeepDeepEquality: KeepDeepEqualityCall<PastePostActionMenuData> =
  combine7EqualityCalls(
    (data) => data.dataWithPropsPreserved,
    ElementPasteWithMetadataKeepDeepEquality,
    (data) => data.dataWithPropsReplaced,
    nullableDeepEquality(ElementPasteWithMetadataKeepDeepEquality),
    (data) => data.targetOriginalPathTrees,
    ElementPathTreesKeepDeepEquality(),
    (data) => data.pasteTargetsToIgnore,
    ElementPathArrayKeepDeepEquality,
    (data) => data.canvasViewportCenter,
    CanvasPointKeepDeepEquality,
    (data) => data.originalAllElementProps,
    AllElementPropsKeepDeepEquality,
    (data) => data.target,
    (_, newValue) => keepDeepEqualityResult(newValue, false),
    (
      dataWithPropsPreserved,
      dataWithPropsReplaced,
      targetOriginalPathTrees,
      pasteTargetsToIgnore,
      canvasViewportCenter,
      originalAllElementProps,
      target,
    ) => ({
      type: 'PASTE',
      target: target,
      dataWithPropsPreserved: dataWithPropsPreserved,
      dataWithPropsReplaced: dataWithPropsReplaced,
      targetOriginalPathTrees: targetOriginalPathTrees,
      pasteTargetsToIgnore: pasteTargetsToIgnore,
      canvasViewportCenter: canvasViewportCenter,
      originalAllElementProps: originalAllElementProps,
    }),
  )

export const PasteHerePostActionMenuDataKeepDeepEquality: KeepDeepEqualityCall<PasteHerePostActionMenuData> =
  combine2EqualityCalls(
    (menudata) => menudata.position,
    CanvasPointKeepDeepEquality,
    (menudata) => menudata.internalClipboard,
    InternalClipboardKeepDeepEquality,
    (position, clipboard) => ({
      type: 'PASTE_HERE',
      position: position,
      internalClipboard: clipboard,
    }),
  )
export const PasteToReplacePostActionMenuDataKeepDeepEquality: KeepDeepEqualityCall<PasteToReplacePostActionMenuData> =
  combine2EqualityCalls(
    (menudata) => menudata.targets,
    ElementPathArrayKeepDeepEquality,
    (menudata) => menudata.internalClipboard,
    InternalClipboardKeepDeepEquality,
    (targets, clipboard) => ({
      type: 'PASTE_TO_REPLACE',
      targets: targets,
      internalClipboard: clipboard,
    }),
  )
export const NavigatorReparentPostActionMenuDataKeepDeepEquality: KeepDeepEqualityCall<NavigatorReparentPostActionMenuData> =
  combine6EqualityCalls(
    (menudata) => menudata.dragSources,
    ElementPathArrayKeepDeepEquality,
    (menudata) => menudata.targetParent,
    ElementPathKeepDeepEquality,
    (menudata) => menudata.indexPosition,
    IndexPositionKeepDeepEquality,
    (menudata) => menudata.canvasViewportCenter,
    CanvasPointKeepDeepEquality,
    (menudata) => menudata.jsxMetadata,
    ElementInstanceMetadataMapKeepDeepEquality,
    (menudata) => menudata.allElementProps,
    AllElementPropsKeepDeepEquality,
    (
      dragSources,
      targetParent,
      indexPosition,
      canvasViewportCenter,
      jsxMetadata,
      allElementProps,
    ) => ({
      type: 'NAVIGATOR_REPARENT',
      dragSources: dragSources,
      targetParent: targetParent,
      indexPosition: indexPosition,
      canvasViewportCenter: canvasViewportCenter,
      jsxMetadata: jsxMetadata,
      allElementProps: allElementProps,
    }),
  )

export const PostActionMenuDataKeepDeepEquality: KeepDeepEqualityCall<PostActionMenuData> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'PASTE':
      if (newValue.type === oldValue.type) {
        return PastePostActionMenuDataKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'PASTE_HERE':
      if (newValue.type === oldValue.type) {
        return PasteHerePostActionMenuDataKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'PASTE_TO_REPLACE':
      if (newValue.type === oldValue.type) {
        return PasteToReplacePostActionMenuDataKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'NAVIGATOR_REPARENT':
      if (newValue.type === oldValue.type) {
        return NavigatorReparentPostActionMenuDataKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      const _exhaustiveCheck: never = oldValue
      throw new Error(`Unhandled type ${JSON.stringify(oldValue)}`)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const TrueUpGroupElementChangedKeepDeepEquality: KeepDeepEqualityCall<TrueUpGroupElementChanged> =
  combine1EqualityCall(
    (value) => value.target,
    ElementPathKeepDeepEquality,
    trueUpGroupElementChanged,
  )

export const TrueUpChildrenOfGroupChangedKeepDeepEquality: KeepDeepEqualityCall<TrueUpChildrenOfGroupChanged> =
  combine1EqualityCall(
    (value) => value.targetParent,
    ElementPathKeepDeepEquality,
    trueUpChildrenOfGroupChanged,
  )

export const TrueUpHuggingElementKeepDeepEquality: KeepDeepEqualityCall<TrueUpHuggingElement> =
  combine2EqualityCalls(
    (value) => value.target,
    ElementPathKeepDeepEquality,
    (value) => value.frame,
    CanvasRectangleKeepDeepEquality,
    (target, frame) => trueUpHuggingElement(target, frame),
  )

export const TrueUpTargetKeepDeepEquality: KeepDeepEqualityCall<TrueUpTarget> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'TRUE_UP_GROUP_ELEMENT_CHANGED':
      if (oldValue.type === newValue.type) {
        return TrueUpGroupElementChangedKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'TRUE_UP_CHILDREN_OF_GROUP_CHANGED':
      if (oldValue.type === newValue.type) {
        return TrueUpChildrenOfGroupChangedKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'TRUE_UP_HUGGING_ELEMENT':
      if (oldValue.type === newValue.type) {
        return TrueUpHuggingElementKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const ActiveFrameTargetPathKeepDeepEquality: KeepDeepEqualityCall<ActiveFrameTargetPath> =
  combine1EqualityCall(
    (data) => data.path,
    ElementPathKeepDeepEquality,
    (path) => ({ type: 'ACTIVE_FRAME_TARGET_PATH', path }),
  )

export const ActiveFrameTargetRectKeepDeepEquality: KeepDeepEqualityCall<ActiveFrameTargetRect> =
  combine1EqualityCall(
    (data) => data.rect,
    CanvasRectangleKeepDeepEquality,
    (rect) => ({ type: 'ACTIVE_FRAME_TARGET_RECT', rect }),
  )

export const ActiveFrameTargetKeepDeepEquality: KeepDeepEqualityCall<ActiveFrameTarget> = (
  oldValue,
  newValue,
) => {
  switch (oldValue.type) {
    case 'ACTIVE_FRAME_TARGET_PATH':
      if (oldValue.type === newValue.type) {
        return ActiveFrameTargetPathKeepDeepEquality(oldValue, newValue)
      }
      break
    case 'ACTIVE_FRAME_TARGET_RECT':
      if (oldValue.type === newValue.type) {
        return ActiveFrameTargetRectKeepDeepEquality(oldValue, newValue)
      }
      break
    default:
      assertNever(oldValue)
  }
  return keepDeepEqualityResult(newValue, false)
}

export const FrameOrPathKeepDeepEquality: KeepDeepEqualityCall<ActiveFrame> = combine3EqualityCalls(
  (data) => data.target,
  ActiveFrameTargetKeepDeepEquality,
  (data) => data.action,
  createCallWithTripleEquals(),
  (data) => data.source,
  CanvasRectangleKeepDeepEquality,
  (target, action, source) => ({ target, action, source }),
)

export const EditorStateKeepDeepEquality: KeepDeepEqualityCall<EditorState> = (
  oldValue,
  newValue,
) => {
  if (oldValue === newValue) {
    return keepDeepEqualityResult(oldValue, true)
  }

  const idResult = NullableStringKeepDeepEquality(oldValue.id, newValue.id)
  const forkedFromProjectIdResult = NullableStringKeepDeepEquality(
    oldValue.forkedFromProjectId,
    newValue.forkedFromProjectId,
  )
  const appIDResult = NullableStringKeepDeepEquality(oldValue.appID, newValue.appID)
  const projectNameResult = StringKeepDeepEquality(oldValue.projectName, newValue.projectName)
  const projectDescriptionResult = StringKeepDeepEquality(
    oldValue.projectDescription,
    newValue.projectDescription,
  )
  const projectVersionResult = NumberKeepDeepEquality(
    oldValue.projectVersion,
    newValue.projectVersion,
  )
  const isLoadedResult = BooleanKeepDeepEquality(oldValue.isLoaded, newValue.isLoaded)

  const trueUpElementsAfterDomWalkerRunsResult = arrayDeepEquality(TrueUpTargetKeepDeepEquality)(
    oldValue.trueUpElementsAfterDomWalkerRuns,
    newValue.trueUpElementsAfterDomWalkerRuns,
  )

  const spyMetadataResult = ElementInstanceMetadataMapKeepDeepEquality(
    oldValue.spyMetadata,
    newValue.spyMetadata,
  )
  const domMetadataResult = ElementInstanceMetadataMapKeepDeepEquality(
    oldValue.domMetadata,
    newValue.domMetadata,
  )
  const jsxMetadataResult = ElementInstanceMetadataMapKeepDeepEquality(
    oldValue.jsxMetadata,
    newValue.jsxMetadata,
  )
  const elementPathTreeResult = ElementPathTreesKeepDeepEquality()(
    oldValue.elementPathTree,
    newValue.elementPathTree,
  )
  const projectContentsResult = ProjectContentTreeRootKeepDeepEquality()(
    oldValue.projectContents,
    newValue.projectContents,
  )
  const codeResultCacheResult = CodeResultCacheKeepDeepEquality(
    oldValue.codeResultCache,
    newValue.codeResultCache,
  )
  const propertyControlsInfoResult = PropertyControlsInfoKeepDeepEquality(
    oldValue.propertyControlsInfo,
    newValue.propertyControlsInfo,
  )
  const nodeModulesResult = EditorStateNodeModulesKeepDeepEquality(
    oldValue.nodeModules,
    newValue.nodeModules,
  )
  const selectedViewsResult = ElementPathArrayKeepDeepEquality(
    oldValue.selectedViews,
    newValue.selectedViews,
  )
  const highlightedViewsResult = ElementPathArrayKeepDeepEquality(
    oldValue.highlightedViews,
    newValue.highlightedViews,
  )
  const hoveredViewsResult = ElementPathArrayKeepDeepEquality(
    oldValue.hoveredViews,
    newValue.hoveredViews,
  )
  const hiddenInstancesResult = ElementPathArrayKeepDeepEquality(
    oldValue.hiddenInstances,
    newValue.hiddenInstances,
  )

  const displayNoneInstancesResult = ElementPathArrayKeepDeepEquality(
    oldValue.displayNoneInstances,
    newValue.displayNoneInstances,
  )

  const warnedInstancesResult = ElementPathArrayKeepDeepEquality(
    oldValue.warnedInstances,
    newValue.warnedInstances,
  )
  const lockedElementsResult = EditorStateLockedElementsDeepEquality(
    oldValue.lockedElements,
    newValue.lockedElements,
  )
  const modeResult = ModeKeepDeepEquality(oldValue.mode, newValue.mode)

  const focusedPanelResult = createCallWithTripleEquals<EditorPanel | null>()(
    oldValue.focusedPanel,
    newValue.focusedPanel,
  )
  const keysPressedResult = objectDeepEquality(BooleanKeepDeepEquality)(
    oldValue.keysPressed,
    newValue.keysPressed,
  )
  const mouseButtonsPressedResult = getIntrospectiveKeepDeepResult<MouseButtonsPressed>(
    oldValue.mouseButtonsPressed,
    newValue.mouseButtonsPressed,
  )
  const openPopupIdResult = NullableStringKeepDeepEquality(
    oldValue.openPopupId,
    newValue.openPopupId,
  )
  const toastsResults = readOnlyArrayDeepEquality(NoticeKeepDeepEquality)(
    oldValue.toasts,
    newValue.toasts,
  )
  const canvasCursorResults = CanvasCursorKeepDeepEquality(
    oldValue.cursorStack,
    newValue.cursorStack,
  )
  const leftMenuResults = EditorStateLeftMenuKeepDeepEquality(oldValue.leftMenu, newValue.leftMenu)
  const rightMenuResults = EditorStateRightMenuKeepDeepEquality(
    oldValue.rightMenu,
    newValue.rightMenu,
  )
  const interfaceDesignerResults = EditorStateInterfaceDesignerKeepDeepEquality(
    oldValue.interfaceDesigner,
    newValue.interfaceDesigner,
  )
  const canvasResults = EditorStateCanvasKeepDeepEquality(oldValue.canvas, newValue.canvas)
  const inspectorResults = EditorStateInspectorKeepDeepEquality(
    oldValue.inspector,
    newValue.inspector,
  )
  const fileBrowserResults = EditorStateFileBrowserKeepDeepEquality(
    oldValue.fileBrowser,
    newValue.fileBrowser,
  )
  const dependencyListResults = EditorStateDependencyListKeepDeepEquality(
    oldValue.dependencyList,
    newValue.dependencyList,
  )
  const genericExternalResourcesResults = EditorStateGenericExternalResourcesKeepDeepEquality(
    oldValue.genericExternalResources,
    newValue.genericExternalResources,
  )
  const googleFontsResourcesResults = EditorStateGoogleFontsResourcesKeepDeepEquality(
    oldValue.googleFontsResources,
    newValue.googleFontsResources,
  )
  const projectSettingsResults = EditorStateProjectSettingsKeepDeepEquality(
    oldValue.projectSettings,
    newValue.projectSettings,
  )
  const navigatorResults = NavigatorStateKeepDeepEquality(oldValue.navigator, newValue.navigator)
  const topmenuResults = EditorStateTopMenuKeepDeepEquality(oldValue.topmenu, newValue.topmenu)
  const previewResults = EditorStatePreviewKeepDeepEquality(oldValue.preview, newValue.preview)
  const homeResults = EditorStateHomeKeepDeepEquality(oldValue.home, newValue.home)
  const lastUsedFontResults = nullableDeepEquality(FontSettingsKeepDeepEquality)(
    oldValue.lastUsedFont,
    newValue.lastUsedFont,
  )
  const modalResults = nullableDeepEquality(ModalDialogKeepDeepEquality)(
    oldValue.modal,
    newValue.modal,
  )
  const localProjectListResults = arrayDeepEquality(ProjectListingKeepDeepEquality)(
    oldValue.localProjectList,
    newValue.localProjectList,
  )
  const projectListResults = arrayDeepEquality(ProjectListingKeepDeepEquality)(
    oldValue.projectList,
    newValue.projectList,
  )
  const showcaseProjectsResults = arrayDeepEquality(ProjectListingKeepDeepEquality)(
    oldValue.showcaseProjects,
    newValue.showcaseProjects,
  )
  const codeEditorErrorsResults = EditorStateCodeEditorErrorsKeepDeepEquality(
    oldValue.codeEditorErrors,
    newValue.codeEditorErrors,
  )
  const thumbnailLastGeneratedResults = NumberKeepDeepEquality(
    oldValue.thumbnailLastGenerated,
    newValue.thumbnailLastGenerated,
  )
  const pasteTargetsToIgnoreResults = ElementPathArrayKeepDeepEquality(
    oldValue.pasteTargetsToIgnore,
    newValue.pasteTargetsToIgnore,
  )
  const parseOrPrintInFlightResults = BooleanKeepDeepEquality(
    oldValue.parseOrPrintInFlight,
    newValue.parseOrPrintInFlight,
  )
  const previousParseOrPrintSkipped = BooleanKeepDeepEquality(
    oldValue.previousParseOrPrintSkipped,
    newValue.previousParseOrPrintSkipped,
  )
  const safeModeResults = BooleanKeepDeepEquality(oldValue.safeMode, newValue.safeMode)
  const saveErrorResults = BooleanKeepDeepEquality(oldValue.saveError, newValue.saveError)
  const vscodeBridgeReadyResults = BooleanKeepDeepEquality(
    oldValue.vscodeBridgeReady,
    newValue.vscodeBridgeReady,
  )
  const vscodeReadyResults = BooleanKeepDeepEquality(oldValue.vscodeReady, newValue.vscodeReady)
  const focusedElementPathResults = nullableDeepEquality(ElementPathKeepDeepEquality)(
    oldValue.focusedElementPath,
    newValue.focusedElementPath,
  )
  const configResults = UtopiaVSCodeConfigKeepDeepEquality(oldValue.config, newValue.config)
  const vscodeLoadingScreenVisibleResults = BooleanKeepDeepEquality(
    oldValue.vscodeLoadingScreenVisible,
    newValue.vscodeLoadingScreenVisible,
  )
  const indexedDBFailedResults = BooleanKeepDeepEquality(
    oldValue.indexedDBFailed,
    newValue.indexedDBFailed,
  )
  const forceParseFilesResults = arrayDeepEquality(StringKeepDeepEquality)(
    oldValue.forceParseFiles,
    newValue.forceParseFiles,
  )
  const allElementPropsResults = AllElementPropsKeepDeepEquality(
    oldValue.allElementProps,
    newValue.allElementProps,
  )
  const currentAllElementPropsResults = AllElementPropsKeepDeepEquality(
    oldValue.currentAllElementProps,
    newValue.currentAllElementProps,
  )

  const variablesInScopeResult = VariablesInScopeKeepDeepEquality(
    oldValue.variablesInScope,
    newValue.variablesInScope,
  )

  const currentVariablesInScopeResult = VariablesInScopeKeepDeepEquality(
    oldValue.currentVariablesInScope,
    newValue.currentVariablesInScope,
  )

  const githubSettingsResults = ProjectGithubSettingsKeepDeepEquality(
    oldValue.githubSettings,
    newValue.githubSettings,
  )

  const imageDragSessionStateEqual = DragSessionStateKeepDeepEquality(
    oldValue.imageDragSessionState,
    newValue.imageDragSessionState,
  )

  const githubOperationsResults = GithubOperationsKeepDeepEquality(
    oldValue.githubOperations,
    newValue.githubOperations,
  )

  const branchContentsResults = nullableDeepEquality(ProjectContentTreeRootKeepDeepEquality())(
    oldValue.branchOriginContents,
    newValue.branchOriginContents,
  )

  const githubDataResults = GithubDataKeepDeepEquality(oldValue.githubData, newValue.githubData)

  const refreshingDependenciesResults = BooleanKeepDeepEquality(
    oldValue.refreshingDependencies,
    newValue.refreshingDependencies,
  )

  const colorSwatchesResults = arrayDeepEquality(ColorSwatchDeepEquality)(
    oldValue.colorSwatches,
    newValue.colorSwatches,
  )
  const internalClipboardResults = InternalClipboardKeepDeepEquality(
    oldValue.internalClipboard,
    newValue.internalClipboard,
  )
  const filesModifiedByAnotherUserResults = arrayDeepEquality(StringKeepDeepEquality)(
    oldValue.filesModifiedByAnotherUser,
    newValue.filesModifiedByAnotherUser,
  )

  const activeFramesResults = arrayDeepEquality(FrameOrPathKeepDeepEquality)(
    oldValue.activeFrames,
    newValue.activeFrames,
  )

  const commentFilterModeResults = createCallWithTripleEquals<CommentFilterMode>()(
    oldValue.commentFilterMode,
    newValue.commentFilterMode,
  )

  const forkingResults = BooleanKeepDeepEquality(oldValue.forking, newValue.forking)

  const collaboratorsResults = arrayDeepEquality(CollaboratorKeepDeepEquality)(
    oldValue.collaborators,
    newValue.collaborators,
  )

  const sharingDialogOpenResults = BooleanKeepDeepEquality(
    oldValue.sharingDialogOpen,
    newValue.sharingDialogOpen,
  )

  const areEqual =
    idResult.areEqual &&
    forkedFromProjectIdResult.areEqual &&
    appIDResult.areEqual &&
    projectNameResult.areEqual &&
    projectDescriptionResult.areEqual &&
    projectVersionResult.areEqual &&
    isLoadedResult.areEqual &&
    trueUpElementsAfterDomWalkerRunsResult.areEqual &&
    spyMetadataResult.areEqual &&
    domMetadataResult.areEqual &&
    jsxMetadataResult.areEqual &&
    elementPathTreeResult.areEqual &&
    projectContentsResult.areEqual &&
    codeResultCacheResult.areEqual &&
    propertyControlsInfoResult.areEqual &&
    nodeModulesResult.areEqual &&
    selectedViewsResult.areEqual &&
    highlightedViewsResult.areEqual &&
    hoveredViewsResult.areEqual &&
    hiddenInstancesResult.areEqual &&
    displayNoneInstancesResult.areEqual &&
    warnedInstancesResult.areEqual &&
    lockedElementsResult.areEqual &&
    modeResult.areEqual &&
    focusedPanelResult.areEqual &&
    keysPressedResult.areEqual &&
    mouseButtonsPressedResult.areEqual &&
    openPopupIdResult.areEqual &&
    toastsResults.areEqual &&
    canvasCursorResults.areEqual &&
    leftMenuResults.areEqual &&
    rightMenuResults.areEqual &&
    interfaceDesignerResults.areEqual &&
    canvasResults.areEqual &&
    inspectorResults.areEqual &&
    fileBrowserResults.areEqual &&
    dependencyListResults.areEqual &&
    genericExternalResourcesResults.areEqual &&
    googleFontsResourcesResults.areEqual &&
    projectSettingsResults.areEqual &&
    navigatorResults.areEqual &&
    topmenuResults.areEqual &&
    previewResults.areEqual &&
    homeResults.areEqual &&
    lastUsedFontResults.areEqual &&
    modalResults.areEqual &&
    localProjectListResults.areEqual &&
    projectListResults.areEqual &&
    showcaseProjectsResults.areEqual &&
    codeEditorErrorsResults.areEqual &&
    thumbnailLastGeneratedResults.areEqual &&
    pasteTargetsToIgnoreResults.areEqual &&
    parseOrPrintInFlightResults.areEqual &&
    previousParseOrPrintSkipped.areEqual &&
    safeModeResults.areEqual &&
    saveErrorResults.areEqual &&
    vscodeBridgeReadyResults.areEqual &&
    vscodeReadyResults.areEqual &&
    focusedElementPathResults.areEqual &&
    configResults.areEqual &&
    vscodeLoadingScreenVisibleResults.areEqual &&
    indexedDBFailedResults.areEqual &&
    forceParseFilesResults.areEqual &&
    allElementPropsResults.areEqual &&
    currentAllElementPropsResults.areEqual &&
    variablesInScopeResult.areEqual &&
    githubSettingsResults.areEqual &&
    imageDragSessionStateEqual.areEqual &&
    githubOperationsResults.areEqual &&
    branchContentsResults.areEqual &&
    githubDataResults.areEqual &&
    refreshingDependenciesResults.areEqual &&
    colorSwatchesResults.areEqual &&
    internalClipboardResults.areEqual &&
    filesModifiedByAnotherUserResults.areEqual &&
    activeFramesResults.areEqual &&
    commentFilterModeResults.areEqual &&
    forkingResults.areEqual &&
    collaboratorsResults.areEqual &&
    sharingDialogOpenResults.areEqual

  if (areEqual) {
    return keepDeepEqualityResult(oldValue, true)
  } else {
    const newEditorState = editorState(
      idResult.value,
      forkedFromProjectIdResult.value,
      appIDResult.value,
      projectNameResult.value,
      projectDescriptionResult.value,
      projectVersionResult.value,
      isLoadedResult.value,
      trueUpElementsAfterDomWalkerRunsResult.value,
      spyMetadataResult.value,
      domMetadataResult.value,
      jsxMetadataResult.value,
      elementPathTreeResult.value,
      projectContentsResult.value,
      codeResultCacheResult.value,
      propertyControlsInfoResult.value,
      nodeModulesResult.value,
      selectedViewsResult.value,
      highlightedViewsResult.value,
      hoveredViewsResult.value,
      hiddenInstancesResult.value,
      displayNoneInstancesResult.value,
      warnedInstancesResult.value,
      lockedElementsResult.value,
      modeResult.value,
      focusedPanelResult.value,
      keysPressedResult.value,
      mouseButtonsPressedResult.value,
      openPopupIdResult.value,
      toastsResults.value,
      canvasCursorResults.value,
      leftMenuResults.value,
      rightMenuResults.value,
      interfaceDesignerResults.value,
      canvasResults.value,
      inspectorResults.value,
      fileBrowserResults.value,
      dependencyListResults.value,
      genericExternalResourcesResults.value,
      googleFontsResourcesResults.value,
      projectSettingsResults.value,
      navigatorResults.value,
      topmenuResults.value,
      previewResults.value,
      homeResults.value,
      lastUsedFontResults.value,
      modalResults.value,
      localProjectListResults.value,
      projectListResults.value,
      showcaseProjectsResults.value,
      codeEditorErrorsResults.value,
      thumbnailLastGeneratedResults.value,
      pasteTargetsToIgnoreResults.value,
      parseOrPrintInFlightResults.value,
      previousParseOrPrintSkipped.value,
      safeModeResults.value,
      saveErrorResults.value,
      vscodeBridgeReadyResults.value,
      vscodeReadyResults.value,
      focusedElementPathResults.value,
      configResults.value,
      vscodeLoadingScreenVisibleResults.value,
      indexedDBFailedResults.value,
      forceParseFilesResults.value,
      allElementPropsResults.value,
      currentAllElementPropsResults.value,
      variablesInScopeResult.value,
      currentVariablesInScopeResult.value,
      githubSettingsResults.value,
      imageDragSessionStateEqual.value,
      githubOperationsResults.value,
      branchContentsResults.value,
      githubDataResults.value,
      refreshingDependenciesResults.value,
      colorSwatchesResults.value,
      internalClipboardResults.value,
      filesModifiedByAnotherUserResults.value,
      activeFramesResults.value,
      commentFilterModeResults.value,
      forkingResults.value,
      collaboratorsResults.value,
      sharingDialogOpenResults.value,
    )

    return keepDeepEqualityResult(newEditorState, false)
  }
}
