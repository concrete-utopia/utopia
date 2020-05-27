import {
  Node,
  QuestionToken,
  ExclamationToken,
  EnumMember,
  SpreadAssignment,
  ShorthandPropertyAssignment,
  PropertyAssignment,
  CatchClause,
  HeritageClause,
  DefaultClause,
  CaseClause,
  ExternalModuleReference,
  ExportSpecifier,
  NamedExports,
  ExportDeclaration,
  ExportAssignment,
  ImportSpecifier,
  NamedImports,
  NamespaceImport,
  ImportClause,
  ImportDeclaration,
  ImportEqualsDeclaration,
  NamespaceExportDeclaration,
  CaseBlock,
  ModuleBlock,
  ModuleDeclaration,
  EnumDeclaration,
  TypeAliasDeclaration,
  InterfaceDeclaration,
  ClassDeclaration,
  FunctionDeclaration,
  VariableDeclarationList,
  VariableDeclaration,
  DebuggerStatement,
  TryStatement,
  ThrowStatement,
  LabeledStatement,
  SwitchStatement,
  WithStatement,
  ReturnStatement,
  BreakStatement,
  ContinueStatement,
  ForOfStatement,
  ForInStatement,
  ForStatement,
  WhileStatement,
  DoStatement,
  IfStatement,
  EmptyStatement,
  VariableStatement,
  Block,
  SemicolonClassElement,
  TemplateSpan,
  MetaProperty,
  NonNullExpression,
  AsExpression,
  ExpressionWithTypeArguments,
  ClassExpression,
  SpreadElement,
  YieldExpression,
  TemplateExpression,
  ConditionalExpression,
  BinaryExpression,
  AwaitExpression,
  VoidExpression,
  TypeOfExpression,
  DeleteExpression,
  ArrowFunction,
  FunctionExpression,
  ParenthesizedExpression,
  TypeAssertion,
  TaggedTemplateExpression,
  NewExpression,
  CallExpression,
  ElementAccessExpression,
  PropertyAccessExpression,
  ObjectLiteralExpression,
  ArrayLiteralExpression,
  BindingElement,
  ArrayBindingPattern,
  ObjectBindingPattern,
  ImportTypeNode,
  LiteralTypeNode,
  MappedTypeNode,
  IndexedAccessTypeNode,
  TypeOperatorNode,
  ThisTypeNode,
  ParenthesizedTypeNode,
  InferTypeNode,
  ConditionalTypeNode,
  IntersectionTypeNode,
  UnionTypeNode,
  RestTypeNode,
  OptionalTypeNode,
  TupleTypeNode,
  ArrayTypeNode,
  TypeLiteralNode,
  TypeQueryNode,
  ConstructorTypeNode,
  FunctionTypeNode,
  TypeReferenceNode,
  TypePredicateNode,
  IndexSignatureDeclaration,
  ConstructSignatureDeclaration,
  ConstructorDeclaration,
  GetAccessorDeclaration,
  SetAccessorDeclaration,
  CallSignatureDeclaration,
  MethodDeclaration,
  MethodSignature,
  PropertyDeclaration,
  PropertySignature,
  Decorator,
  ParameterDeclaration,
  SyntaxKind,
  Token,
  TypeParameterDeclaration,
  ComputedPropertyName,
  NumericLiteral,
  BigIntLiteral,
  StringLiteral,
  RegularExpressionLiteral,
  NoSubstitutionTemplateLiteral,
  TemplateHead,
  TemplateMiddle,
  TemplateTail,
  Identifier,
  QualifiedName,
  ExpressionStatement,
  PrefixUnaryExpression,
  PostfixUnaryExpression,
  JsxExpression,
  JsxSpreadAttribute,
  JsxAttributes,
  JsxAttribute,
  JsxClosingFragment,
  JsxOpeningFragment,
  JsxFragment,
  JsxClosingElement,
  JsxOpeningElement,
  JsxSelfClosingElement,
  JsxElement,
  SourceFile,
  createStringLiteral,
  createPropertyAccess,
  createIdentifier,
  ScriptTarget,
  createParen,
  createAsExpression,
  createTypeReferenceNode,
  createQualifiedName,
  KeywordTypeNode,
  Modifier,
  NodeArray,
  Expression,
  createArrayLiteral,
  updateSourceFileNode,
  createExpressionStatement,
  JsxText,
} from 'typescript'

import {
  createTsCall,
  createBooleanLiteral,
  createNodeFlags,
  createLiteralCall,
  createTsAccess,
  transformInternalSyntaxKind,
} from './helper'

interface QuestionOrExclamation {
  questionToken?: QuestionToken
  exclamationToken?: ExclamationToken
}

function generateEnumMember(node: EnumMember) {
  return createTsCall('createEnumMember', [
    transformVisitor(node.name),
    transformVisitor(node.initializer),
  ])
}

function generateSpreadAssignment(node: SpreadAssignment) {
  return createTsCall('createSpreadAssignment', [transformVisitor(node.expression)])
}

function generateShorthandPropertyAssignment(node: ShorthandPropertyAssignment) {
  return createTsCall('createShorthandPropertyAssignment', [
    transformVisitor(node.name),
    transformVisitor(node.objectAssignmentInitializer),
  ])
}

function generatePropertyAssignment(node: PropertyAssignment) {
  return createTsCall('createPropertyAssignment', [
    transformVisitor(node.name),
    transformVisitor(node.initializer),
  ])
}

function generateCatchClause(node: CatchClause) {
  return createTsCall('createCatchClause', [
    transformVisitor(node.variableDeclaration),
    transformVisitor(node.block),
  ])
}

function generateHeritageClause(node: HeritageClause) {
  return createTsCall('createHeritageClause', [
    transformSyntaxKind(node.token),
    transformVisitors(node.types),
  ])
}

function generateDefaultClause(node: DefaultClause) {
  return createTsCall('createDefaultClause', [transformVisitors(node.statements)])
}

function generateCaseClause(node: CaseClause) {
  return createTsCall('createCaseClause', [
    transformVisitor(node.expression),
    transformVisitors(node.statements),
  ])
}

function generateExternalModuleReference(node: ExternalModuleReference) {
  return createTsCall('createExternalModuleReference', [transformVisitor(node.expression)])
}

function generateExportSpecifier(node: ExportSpecifier) {
  return createTsCall('createExportSpecifier', [
    transformVisitor(node.propertyName),
    transformVisitor(node.name),
  ])
}

function generateNamedExports(node: NamedExports) {
  return createTsCall('createNamedExports', [transformVisitors(node.elements)])
}

function generateExportDeclaration(node: ExportDeclaration) {
  return createTsCall('createExportDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.exportClause),
    transformVisitor(node.moduleSpecifier),
  ])
}

function generateExportAssignment(node: ExportAssignment) {
  return createTsCall('createExportAssignment', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    createBooleanLiteral(node.isExportEquals),
    transformVisitor(node.expression),
  ])
}

function generateImportSpecifier(node: ImportSpecifier) {
  return createTsCall('createImportSpecifier', [
    transformVisitor(node.propertyName),
    transformVisitor(node.name),
  ])
}

function generateNamedImports(node: NamedImports) {
  return createTsCall('createNamedImports', [transformVisitors(node.elements)])
}

function generateNamespaceImport(node: NamespaceImport) {
  return createTsCall('createNamespaceImport', [transformVisitor(node.name)])
}

function generateImportClause(node: ImportClause) {
  return createTsCall('createImportClause', [
    transformVisitor(node.name),
    transformVisitor(node.namedBindings),
  ])
}

function generateImportDeclaration(node: ImportDeclaration) {
  return createTsCall('createImportDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.importClause),
    transformVisitor(node.moduleSpecifier),
  ])
}

function generateImportEqualsDeclaration(node: ImportEqualsDeclaration) {
  return createTsCall('createImportEqualsDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitor(node.moduleReference),
  ])
}

function generateNamespaceExportDeclaration(node: NamespaceExportDeclaration) {
  return createTsCall('createNamespaceExportDeclaration', [transformVisitor(node.name)])
}

function generateCaseBlock(node: CaseBlock) {
  return createTsCall('createCaseBlock', [transformVisitors(node.clauses)])
}

function generateModuleBlock(node: ModuleBlock) {
  return createTsCall('createModuleBlock', [transformVisitors(node.statements)])
}

function generateModuleDeclaration(node: ModuleDeclaration) {
  return createTsCall('createModuleDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitor(node.body),
    createNodeFlags(node.flags),
  ])
}

function generateEnumDeclaration(node: EnumDeclaration) {
  return createTsCall('createEnumDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.members),
  ])
}

function generateTypeAliasDeclaration(node: TypeAliasDeclaration) {
  return createTsCall('createTypeAliasDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.typeParameters),
    transformVisitor(node.type),
  ])
}

function generateInterfaceDeclaration(node: InterfaceDeclaration) {
  return createTsCall('createInterfaceDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.typeParameters),
    transformVisitors(node.heritageClauses),
    transformVisitors(node.members),
  ])
}

function generateClassDeclaration(node: ClassDeclaration) {
  return createTsCall('createClassDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.typeParameters),
    transformVisitors(node.heritageClauses),
    transformVisitors(node.members),
  ])
}

function generateFunctionDeclaration(node: FunctionDeclaration) {
  return createTsCall('createFunctionDeclaration', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.asteriskToken),
    transformVisitor(node.name),
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
    transformVisitor(node.body),
  ])
}

function generateVariableDeclarationList(node: VariableDeclarationList) {
  return createTsCall('createVariableDeclarationList', [
    transformVisitors(node.declarations),
    createNodeFlags(node.flags),
  ])
}

function generateVariableDeclaration(node: VariableDeclaration) {
  return createTsCall('createVariableDeclaration', [
    transformVisitor(node.name),
    transformVisitor(node.type),
    transformVisitor(node.initializer),
  ])
}

function generateDebuggerStatement(node: DebuggerStatement) {
  return createTsCall('createDebuggerStatement', [])
}

function generateTryStatement(node: TryStatement) {
  return createTsCall('createTry', [
    transformVisitor(node.tryBlock),
    transformVisitor(node.catchClause),
    transformVisitor(node.finallyBlock),
  ])
}

function generateThrowStatement(node: ThrowStatement) {
  return createTsCall('createThrow', [transformVisitor(node.expression)])
}

function generateLabeledStatement(node: LabeledStatement) {
  return createTsCall('createLabel', [
    transformVisitor(node.label),
    transformVisitor(node.statement),
  ])
}

function generateSwitchStatement(node: SwitchStatement) {
  return createTsCall('createSwitch', [
    transformVisitor(node.expression),
    transformVisitor(node.caseBlock),
  ])
}

function generateWithStatement(node: WithStatement) {
  return createTsCall('createWith', [
    transformVisitor(node.expression),
    transformVisitor(node.statement),
  ])
}

function generateReturnStatement(node: ReturnStatement) {
  return createTsCall('createReturn', [transformVisitor(node.expression)])
}

function generateBreakStatement(node: BreakStatement) {
  return createTsCall('createBreak', [transformVisitor(node.label)])
}

function generateContinueStatement(node: ContinueStatement) {
  return createTsCall('createContinue', [transformVisitor(node.label)])
}

function generateForOfStatement(node: ForOfStatement) {
  return createTsCall('createForOf', [
    transformVisitor(node.awaitModifier),
    transformVisitor(node.initializer),
    transformVisitor(node.expression),
    transformVisitor(node.statement),
  ])
}

function generateForInStatement(node: ForInStatement) {
  return createTsCall('createForIn', [
    transformVisitor(node.initializer),
    transformVisitor(node.expression),
    transformVisitor(node.statement),
  ])
}

function generateForStatement(node: ForStatement) {
  return createTsCall('createFor', [
    transformVisitor(node.initializer),
    transformVisitor(node.condition),
    transformVisitor(node.incrementor),
    transformVisitor(node.statement),
  ])
}

function generateWhileStatement(node: WhileStatement) {
  return createTsCall('createWhile', [
    transformVisitor(node.expression),
    transformVisitor(node.statement),
  ])
}

function generateDoStatement(node: DoStatement) {
  return createTsCall('createDo', [
    transformVisitor(node.statement),
    transformVisitor(node.expression),
  ])
}

function generateIfStatement(node: IfStatement) {
  return createTsCall('createIf', [
    transformVisitor(node.expression),
    transformVisitor(node.thenStatement),
    transformVisitor(node.elseStatement),
  ])
}

function generateEmptyStatement(node: EmptyStatement) {
  return createTsCall('createEmptyStatement', [])
}

function generateVariableStatement(node: VariableStatement) {
  return createTsCall('createVariableStatement', [
    transformVisitors(node.modifiers),
    transformVisitor(node.declarationList),
  ])
}

function generateBlock(node: Block) {
  return createTsCall('createBlock', [
    transformVisitors(node.statements),
    createBooleanLiteral(true),
  ])
}

function generateSemicolonClassElement(node: SemicolonClassElement) {
  return createTsCall('createSemicolonClassElement', [])
}

function generateTemplateSpan(node: TemplateSpan) {
  return createTsCall('createTemplateSpan', [
    transformVisitor(node.expression),
    transformVisitor(node.literal),
  ])
}

function generateMetaProperty(node: MetaProperty) {
  return createTsCall('createMetaProperty', [
    transformSyntaxKind(node.keywordToken),
    transformVisitor(node.name),
  ])
}

function generateNonNullExpression(node: NonNullExpression) {
  return createTsCall('createNonNullExpression', [transformVisitor(node.expression)])
}

function generateAsExpression(node: AsExpression) {
  return createTsCall('createAsExpression', [
    transformVisitor(node.expression),
    transformVisitor(node.type),
  ])
}

function generateExpressionWithTypeArguments(node: ExpressionWithTypeArguments) {
  return createTsCall('createExpressionWithTypeArguments', [
    transformVisitors(node.typeArguments),
    transformVisitor(node.expression),
  ])
}

function generateClassExpression(node: ClassExpression) {
  return createTsCall('createClassExpression', [
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.typeParameters),
    transformVisitors(node.heritageClauses),
    transformVisitors(node.members),
  ])
}

function generateSpreadElement(node: SpreadElement) {
  return createTsCall('createSpread', [transformVisitor(node.expression)])
}

function generateYieldExpression(node: YieldExpression) {
  return createTsCall('createYield', [transformVisitor(node.expression)])
}

function generateTemplateExpression(node: TemplateExpression) {
  return createTsCall('createTemplateExpression', [
    transformVisitor(node.head),
    transformVisitors(node.templateSpans),
  ])
}

function generateConditionalExpression(node: ConditionalExpression) {
  return createTsCall('createConditional', [
    transformVisitor(node.condition),
    transformVisitor(node.whenTrue),
    transformVisitor(node.whenFalse),
  ])
}

function generateBinaryExpression(node: BinaryExpression) {
  return createTsCall('createBinary', [
    transformVisitor(node.left),
    transformVisitor(node.operatorToken),
    transformVisitor(node.right),
  ])
}

function generateAwaitExpression(node: AwaitExpression) {
  return createTsCall('createAwait', [transformVisitor(node.expression)])
}

function generateVoidExpression(node: VoidExpression) {
  return createTsCall('createVoid', [transformVisitor(node.expression)])
}

function generateTypeOfExpression(node: TypeOfExpression) {
  return createTsCall('createTypeOf', [transformVisitor(node.expression)])
}

function generateDeleteExpression(node: DeleteExpression) {
  return createTsCall('createDelete', [transformVisitor(node.expression)])
}

function generateArrowFunction(node: ArrowFunction) {
  return createTsCall('createArrowFunction', [
    transformVisitors(node.modifiers),
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
    transformVisitor(node.equalsGreaterThanToken),
    transformVisitor(node.body),
  ])
}

function generateFunctionExpression(node: FunctionExpression) {
  return createTsCall('createFunctionExpression', [
    transformVisitors(node.modifiers),
    transformVisitor(node.asteriskToken),
    transformVisitor(node.name),
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
    transformVisitor(node.body),
  ])
}

function generateParenthesizedExpression(node: ParenthesizedExpression) {
  return createTsCall('createParen', [transformVisitor(node.expression)])
}

function generateTypeAssertionExpression(node: TypeAssertion) {
  return createTsCall('createTypeAssertion', [
    transformVisitor(node.type),
    transformVisitor(node.expression),
  ])
}

function generateTaggedTemplateExpression(node: TaggedTemplateExpression) {
  return createTsCall('createTaggedTemplate', [
    transformVisitor(node.tag),
    transformVisitor(node.template),
  ])
}

function generateNewExpression(node: NewExpression) {
  return createTsCall('createNew', [
    transformVisitor(node.expression),
    transformVisitors(node.typeArguments),
    transformVisitors(node.arguments),
  ])
}

function generateCallExpression(node: CallExpression) {
  return createTsCall('createCall', [
    transformVisitor(node.expression),
    transformVisitors(node.typeArguments),
    transformVisitors(node.arguments),
  ])
}

function generateElementAccessExpression(node: ElementAccessExpression) {
  return createTsCall('createElementAccess', [
    transformVisitor(node.expression),
    transformVisitor(node.argumentExpression),
  ])
}

function generatePropertyAccessExpression(node: PropertyAccessExpression) {
  return createTsCall('createPropertyAccess', [
    transformVisitor(node.expression),
    transformVisitor(node.name),
  ])
}

function generateObjectLiteralExpression(node: ObjectLiteralExpression) {
  return createTsCall('createObjectLiteral', [
    transformVisitors(node.properties),
    createBooleanLiteral(false),
  ])
}

function generateArrayLiteralExpression(node: ArrayLiteralExpression) {
  return createTsCall('createArrayLiteral', [
    transformVisitors(node.elements),
    createBooleanLiteral(false),
  ])
}

function generateBindingElement(node: BindingElement) {
  return createTsCall('createBindingElement', [
    transformVisitor(node.dotDotDotToken),
    transformVisitor(node.propertyName),
    transformVisitor(node.name),
    transformVisitor(node.initializer),
  ])
}

function generateArrayBindingPattern(node: ArrayBindingPattern) {
  return createTsCall('createArrayBindingPattern', [transformVisitors(node.elements)])
}

function generateObjectBindingPattern(node: ObjectBindingPattern) {
  return createTsCall('createObjectBindingPattern', [transformVisitors(node.elements)])
}

function generateImportType(node: ImportTypeNode) {
  return createTsCall('createImportTypeNode', [
    transformVisitor(node.argument),
    transformVisitor(node.qualifier),
    transformVisitors(node.typeArguments),
    createBooleanLiteral(node.isTypeOf),
  ])
}

function generateLiteralType(node: LiteralTypeNode) {
  return createTsCall('createLiteralTypeNode', [transformVisitor(node.literal)])
}

function generateMappedType(node: MappedTypeNode) {
  return createTsCall('createMappedTypeNode', [
    transformVisitor(node.readonlyToken),
    transformVisitor(node.typeParameter),
    transformVisitor(node.questionToken),
    transformVisitor(node.type),
  ])
}

function generateIndexedAccessType(node: IndexedAccessTypeNode) {
  return createTsCall('createIndexedAccessTypeNode', [
    transformVisitor(node.objectType),
    transformVisitor(node.indexType),
  ])
}

function generateTypeOperator(node: TypeOperatorNode) {
  return createTsCall('createTypeOperatorNode', [
    transformSyntaxKind(node.operator),
    transformVisitor(node.type),
  ])
}

function generateThisType(node: ThisTypeNode) {
  return createTsCall('createThisTypeNode')
}

function generateParenthesizedType(node: ParenthesizedTypeNode) {
  return createTsCall('createParenthesizedType', [transformVisitor(node.type)])
}

function generateInferType(node: InferTypeNode) {
  return createTsCall('createInferTypeNode', [transformVisitor(node.typeParameter)])
}

function generateConditionalType(node: ConditionalTypeNode) {
  return createTsCall('createConditionalTypeNode', [
    transformVisitor(node.checkType),
    transformVisitor(node.extendsType),
    transformVisitor(node.trueType),
    transformVisitor(node.falseType),
  ])
}

function generateIntersectionType(node: IntersectionTypeNode) {
  return createTsCall('createIntersectionTypeNode', [transformVisitors(node.types)])
}

function generateUnionType(node: UnionTypeNode) {
  return createTsCall('createUnionTypeNode', [transformVisitors(node.types)])
}

function generateRestType(node: RestTypeNode) {
  return createTsCall('createRestTypeNode', [transformVisitor(node.type)])
}

function generateOptionalType(node: OptionalTypeNode) {
  return createTsCall('createOptionalTypeNode', [transformVisitor(node.type)])
}

function generateTypleType(node: TupleTypeNode) {
  return createTsCall('createTupleTypeNode', [transformVisitors(node.elementTypes)])
}

function generateArrayType(node: ArrayTypeNode) {
  return createTsCall('createArrayTypeNode', [transformVisitor(node.elementType)])
}

function generateTypeLiteral(node: TypeLiteralNode) {
  return createTsCall('createTypeLiteralNode', [transformVisitors(node.members)])
}

function generateTypeQuery(node: TypeQueryNode) {
  return createTsCall('createTypeQueryNode', [transformVisitor(node.exprName)])
}

function generateConstructorType(node: ConstructorTypeNode) {
  return createTsCall('createConstructorTypeNode', [
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
  ])
}

function generateFunctionType(node: FunctionTypeNode) {
  return createTsCall('createFunctionTypeNode', [
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
  ])
}

function generateTypeReference(node: TypeReferenceNode) {
  return createTsCall('createTypeReferenceNode', [
    transformVisitor(node.typeName),
    transformVisitors(node.typeArguments),
  ])
}

function generateTypePredicate(node: TypePredicateNode) {
  return createTsCall('createTypePredicateNode', [
    transformVisitor(node.parameterName),
    transformVisitor(node.type),
  ])
}

function generateIndexSignature(node: IndexSignatureDeclaration) {
  return createTsCall('createIndexSignature', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
  ])
}

function generateConstructSignature(node: ConstructSignatureDeclaration) {
  return createTsCall('createConstructSignature', [
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
  ])
}

function generateConstructor(node: ConstructorDeclaration) {
  return createTsCall('createConstructor', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitors(node.parameters),
    transformVisitor(node.body),
  ])
}

function generateGetAccessor(node: GetAccessorDeclaration) {
  return createTsCall('createGetAccessor', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
    transformVisitor(node.body),
  ])
}

function generateSetAccessor(node: SetAccessorDeclaration) {
  return createTsCall('createSetAccessor', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitors(node.parameters),
    transformVisitor(node.body),
  ])
}

function generateCallSignature(node: CallSignatureDeclaration) {
  return createTsCall('createCallSignature', [
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
  ])
}

function generateMethodDeclaration(node: MethodDeclaration) {
  return createTsCall('createMethod', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.asteriskToken),
    transformVisitor(node.name),
    transformVisitor(node.questionToken),
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
    transformVisitor(node.body),
  ])
}

function generateMethodSignature(node: MethodSignature) {
  return createTsCall('createMethodSignature', [
    transformVisitors(node.typeParameters),
    transformVisitors(node.parameters),
    transformVisitor(node.type),
    transformVisitor(node.name),
    transformVisitor(node.questionToken),
  ])
}

function generatePropertyDeclaration(node: PropertyDeclaration) {
  return createTsCall('createProperty', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitorQuestionOrExclamation(node),
    transformVisitor(node.type),
    transformVisitor(node.initializer),
  ])
}

function generatePropertySignature(node: PropertySignature) {
  return createTsCall('createPropertySignature', [
    transformVisitors(node.modifiers),
    transformVisitor(node.name),
    transformVisitor(node.questionToken),
    transformVisitor(node.type),
    transformVisitor(node.initializer),
  ])
}

function generateDecorator(node: Decorator) {
  return createTsCall('createDecorator', [transformVisitor(node.expression)])
}

function generateParameter(node: ParameterDeclaration) {
  return createTsCall('createParameter', [
    transformVisitors(node.decorators),
    transformVisitors(node.modifiers),
    transformVisitor(node.dotDotDotToken),
    transformVisitor(node.name),
    transformVisitor(node.questionToken),
    transformVisitor(node.type),
    transformVisitor(node.initializer),
  ])
}

function generateToken<T extends SyntaxKind>(node: Token<T>) {
  return createTsCall('createToken', [transformSyntaxKind(node.kind)])
}

function generateTypeParameter(node: TypeParameterDeclaration) {
  return createTsCall('createTypeParameterDeclaration', [
    transformVisitor(node.name),
    transformVisitor(node.constraint),
    transformVisitor(node.default),
  ])
}

function generateComputedPropertyName(node: ComputedPropertyName) {
  return createTsCall('createComputedPropertyName', [transformVisitor(node.expression)])
}

function generateNumericLiteral(node: NumericLiteral) {
  return createLiteralCall(node, 'createNumericLiteral')
}

// eslint-disable-next-line no-redeclare
function BigIntLiteral(node: BigIntLiteral) {
  return createLiteralCall(node, 'createBigIntLiteral')
}

function generateStringLiteral(node: StringLiteral) {
  return createLiteralCall(node, 'createStringLiteral')
}

function generateRegularExpressionLiteral(node: RegularExpressionLiteral) {
  return createLiteralCall(node, 'createRegularExpressionLiteral')
}

function generateNoSubstitutionTemplateLiteral(node: NoSubstitutionTemplateLiteral) {
  return createLiteralCall(node, 'createNoSubstitutionTemplateLiteral')
}

function generateTemplateHead(node: TemplateHead) {
  return createLiteralCall(node, 'createTemplateHead')
}

function generateTemplateMiddle(node: TemplateMiddle) {
  return createLiteralCall(node, 'createTemplateMiddle')
}

function generateTemplateTail(node: TemplateTail) {
  return createLiteralCall(node, 'createTemplateTail')
}

function generateIdentifier(node: Identifier) {
  return createLiteralCall(node, 'createIdentifier')
}

function generateQualifiedName(node: QualifiedName) {
  return createTsCall('createQualifiedName', [
    transformVisitor(node.left),
    transformVisitor(node.right),
  ])
}

function generateExpressionStatement(node: ExpressionStatement) {
  return createTsCall('createExpressionStatement', [transformVisitor(node.expression)])
}

function generatePrefixUnaryExpression(node: PrefixUnaryExpression) {
  return createTsCall('createPrefix', [
    transformSyntaxKind(node.operator),
    transformVisitor(node.operand),
  ])
}

function generatePostfixUnaryExpression(node: PostfixUnaryExpression) {
  return createTsCall('createPostfix', [
    transformVisitor(node.operand),
    transformSyntaxKind(node.operator),
  ])
}

function generateJsxExpression(node: JsxExpression) {
  return createTsCall('createJsxExpression', [
    transformVisitor(node.dotDotDotToken),
    transformVisitor(node.expression),
  ])
}

function generateJsxSpreadAttribute(node: JsxSpreadAttribute) {
  return createTsCall('createJsxSpreadAttribute', [transformVisitor(node.expression)])
}

function generateJsxAttributes(node: JsxAttributes) {
  return createTsCall('createJsxAttributes', [transformVisitors(node.properties)])
}

function generateJsxAttribute(node: JsxAttribute) {
  return createTsCall('createJsxAttribute', [
    transformVisitor(node.name),
    transformVisitor(node.initializer),
  ])
}

function generateJsxClosingElement(node: JsxClosingElement) {
  return createTsCall('createJsxClosingElement', [transformVisitor(node.tagName)])
}

function generateJsxOpeningElement(node: JsxOpeningElement) {
  return createTsCall('createJsxOpeningElement', [
    transformVisitor(node.tagName),
    transformVisitors(node.typeArguments),
    transformVisitor(node.attributes),
  ])
}

function generateJsxSelfClosingElement(node: JsxSelfClosingElement) {
  return createTsCall('createJsxSelfClosingElement', [
    transformVisitor(node.tagName),
    transformVisitors(node.typeArguments),
    transformVisitor(node.attributes),
  ])
}

function generateJsxElement(node: JsxElement) {
  return createTsCall('createJsxElement', [
    transformVisitor(node.openingElement),
    transformVisitors(node.children),
    transformVisitor(node.closingElement),
  ])
}

function generateJsxFragment(node: JsxFragment) {
  return createTsCall('createJsxFragment', [
    transformVisitor(node.openingFragment),
    transformVisitors(node.children),
    transformVisitor(node.closingFragment),
  ])
}

function generateJsxOpeningFragment(node: JsxOpeningFragment) {
  return createTsCall('createJsxOpeningFragment', [])
}

function generateJsxClosingFragment(node: JsxClosingFragment) {
  return createTsCall('createJsxJsxClosingFragment', [])
}

function generateJsxText(node: JsxText) {
  return createTsCall('createJsxText', [
    createStringLiteral(node.text),
    createBooleanLiteral(node.containsOnlyTriviaWhiteSpaces),
  ])
}

function generateSourceFile(node: SourceFile) {
  return createTsCall('updateSourceFileNode', [
    createTsCall('createSourceFile', [
      createStringLiteral(node.fileName),
      createStringLiteral(''),
      createPropertyAccess(
        createTsAccess(createIdentifier('ScriptTarget')),
        createIdentifier(ScriptTarget[node.languageVersion]),
      ),
    ]),
    transformVisitors(node.statements),
  ])
}

function transformSyntaxKind(kind: SyntaxKind) {
  return createPropertyAccess(
    createTsAccess(createIdentifier('SyntaxKind')),
    createIdentifier(transformInternalSyntaxKind(SyntaxKind[kind])),
  )
}

function transformRawNode(kind: SyntaxKind) {
  return createParen(
    createAsExpression(
      createTsCall('createNode', [transformSyntaxKind(kind)]),
      createTypeReferenceNode(
        createQualifiedName(createIdentifier('ts'), createIdentifier('Token')),
        [
          createTypeReferenceNode(
            createQualifiedName(
              createQualifiedName(createIdentifier('ts'), createIdentifier('SyntaxKind')),
              createIdentifier(SyntaxKind[kind]),
            ),
            undefined,
          ),
        ],
      ),
    ),
  )
}

function transformKeyword(kind: SyntaxKind) {
  switch (kind) {
    case SyntaxKind.TrueKeyword:
      return createTsCall('createTrue', [])
    case SyntaxKind.FalseKeyword:
      return createTsCall('createFalse', [])
    case SyntaxKind.NullKeyword:
      return createTsCall('createNull', [])
    case SyntaxKind.SuperKeyword:
      return createTsCall('createSuper', [])
    case SyntaxKind.ThisKeyword:
      return createTsCall('createThis', [])
    case SyntaxKind.AnyKeyword:
    case SyntaxKind.UnknownKeyword:
    case SyntaxKind.NumberKeyword:
    case SyntaxKind.BigIntKeyword:
    case SyntaxKind.ObjectKeyword:
    case SyntaxKind.BooleanKeyword:
    case SyntaxKind.StringKeyword:
    case SyntaxKind.SymbolKeyword:
    case SyntaxKind.VoidKeyword:
    case SyntaxKind.UndefinedKeyword:
    case SyntaxKind.NeverKeyword:
      return transformTypeKeyword(kind)

    case SyntaxKind.AbstractKeyword:
    case SyntaxKind.AsyncKeyword:
    case SyntaxKind.ConstKeyword:
    case SyntaxKind.DeclareKeyword:
    case SyntaxKind.DefaultKeyword:
    case SyntaxKind.ExportKeyword:
    case SyntaxKind.PublicKeyword:
    case SyntaxKind.PrivateKeyword:
    case SyntaxKind.ProtectedKeyword:
    case SyntaxKind.ReadonlyKeyword:
    case SyntaxKind.StaticKeyword:
      return transformModifier(kind)

    case SyntaxKind.ImportKeyword:
      return transformAsExpression(transformRawNode(kind))

    default:
      return transformRawNode(kind)
  }
}

function transformTypeKeyword(kind: KeywordTypeNode['kind']) {
  return createTsCall('createKeywordTypeNode', [transformSyntaxKind(kind)])
}

function transformModifier(kind: Modifier['kind']) {
  return createTsCall('createModifier', [transformSyntaxKind(kind)])
}

function transformAsExpression(expr: Expression) {
  return createAsExpression(
    expr,
    createTypeReferenceNode(
      createQualifiedName(createIdentifier('ts'), createIdentifier('Expression')),
      undefined,
    ),
  )
}

function transformVisitorQuestionOrExclamation(node: QuestionOrExclamation) {
  if (node.questionToken) {
    return transformVisitor(node.questionToken)
  } else if (node.exclamationToken) {
    return transformVisitor(node.exclamationToken)
  } else {
    return createIdentifier('undefined')
  }
}

function transformVisitors(nodes?: NodeArray<Node>): Expression {
  if (!nodes) {
    return createIdentifier('undefined')
  }
  return createArrayLiteral(nodes.map(transformVisitor))
}

function isToken(kind: SyntaxKind) {
  return kind >= SyntaxKind.FirstToken && kind <= SyntaxKind.LastToken
}

function isKeyword(kind: SyntaxKind) {
  return kind >= SyntaxKind.FirstKeyword && kind <= SyntaxKind.LastKeyword
}

function transformVisitor(node?: Node): Expression {
  if (!node) {
    return createIdentifier('undefined')
  }

  switch (node.kind) {
    case SyntaxKind.NumericLiteral:
      return generateNumericLiteral(node as NumericLiteral)
    case SyntaxKind.BigIntLiteral:
      return BigIntLiteral(node as BigIntLiteral)
    case SyntaxKind.StringLiteral:
      return generateStringLiteral(node as StringLiteral)
    case SyntaxKind.RegularExpressionLiteral:
      return generateRegularExpressionLiteral(node as RegularExpressionLiteral)
    case SyntaxKind.NoSubstitutionTemplateLiteral:
      return generateNoSubstitutionTemplateLiteral(node as NoSubstitutionTemplateLiteral)
    case SyntaxKind.TemplateHead:
      return generateTemplateHead(node as TemplateHead)
    case SyntaxKind.TemplateMiddle:
      return generateTemplateMiddle(node as TemplateMiddle)
    case SyntaxKind.TemplateTail:
      return generateTemplateTail(node as TemplateTail)
    case SyntaxKind.Identifier:
      return generateIdentifier(node as Identifier)

    case SyntaxKind.QualifiedName:
      return generateQualifiedName(node as QualifiedName)
    case SyntaxKind.ComputedPropertyName:
      return generateComputedPropertyName(node as ComputedPropertyName)
    case SyntaxKind.TypeParameter:
      return generateTypeParameter(node as TypeParameterDeclaration)
    case SyntaxKind.Parameter:
      return generateParameter(node as ParameterDeclaration)
    case SyntaxKind.Decorator:
      return generateDecorator(node as Decorator)
    case SyntaxKind.PropertySignature:
      return generatePropertySignature(node as PropertySignature)
    case SyntaxKind.PropertyDeclaration:
      return generatePropertyDeclaration(node as PropertyDeclaration)
    case SyntaxKind.MethodSignature:
      return generateMethodSignature(node as MethodSignature)
    case SyntaxKind.MethodDeclaration:
      return generateMethodDeclaration(node as MethodDeclaration)
    case SyntaxKind.Constructor:
      return generateConstructor(node as ConstructorDeclaration)
    case SyntaxKind.GetAccessor:
      return generateGetAccessor(node as GetAccessorDeclaration)
    case SyntaxKind.SetAccessor:
      return generateSetAccessor(node as SetAccessorDeclaration)
    case SyntaxKind.CallSignature:
      return generateCallSignature(node as CallSignatureDeclaration)
    case SyntaxKind.ConstructSignature:
      return generateConstructSignature(node as ConstructSignatureDeclaration)
    case SyntaxKind.IndexSignature:
      return generateIndexSignature(node as IndexSignatureDeclaration)
    case SyntaxKind.TypePredicate:
      return generateTypePredicate(node as TypePredicateNode)
    case SyntaxKind.TypeReference:
      return generateTypeReference(node as TypeReferenceNode)
    case SyntaxKind.FunctionType:
      return generateFunctionType(node as FunctionTypeNode)
    case SyntaxKind.ConstructorType:
      return generateConstructorType(node as ConstructorTypeNode)
    case SyntaxKind.TypeQuery:
      return generateTypeQuery(node as TypeQueryNode)
    case SyntaxKind.TypeLiteral:
      return generateTypeLiteral(node as TypeLiteralNode)
    case SyntaxKind.ArrayType:
      return generateArrayType(node as ArrayTypeNode)
    case SyntaxKind.TupleType:
      return generateTypleType(node as TupleTypeNode)
    case SyntaxKind.OptionalType:
      return generateOptionalType(node as OptionalTypeNode)
    case SyntaxKind.RestType:
      return generateRestType(node as RestTypeNode)
    case SyntaxKind.UnionType:
      return generateUnionType(node as UnionTypeNode)
    case SyntaxKind.IntersectionType:
      return generateIntersectionType(node as IntersectionTypeNode)
    case SyntaxKind.ConditionalType:
      return generateConditionalType(node as ConditionalTypeNode)
    case SyntaxKind.InferType:
      return generateInferType(node as InferTypeNode)
    case SyntaxKind.ParenthesizedType:
      return generateParenthesizedType(node as ParenthesizedTypeNode)
    case SyntaxKind.ThisType:
      return generateThisType(node as ThisTypeNode)
    case SyntaxKind.TypeOperator:
      return generateTypeOperator(node as TypeOperatorNode)
    case SyntaxKind.IndexedAccessType:
      return generateIndexedAccessType(node as IndexedAccessTypeNode)
    case SyntaxKind.MappedType:
      return generateMappedType(node as MappedTypeNode)
    case SyntaxKind.LiteralType:
      return generateLiteralType(node as LiteralTypeNode)
    case SyntaxKind.ImportType:
      return generateImportType(node as ImportTypeNode)
    case SyntaxKind.ObjectBindingPattern:
      return generateObjectBindingPattern(node as ObjectBindingPattern)
    case SyntaxKind.ArrayBindingPattern:
      return generateArrayBindingPattern(node as ArrayBindingPattern)
    case SyntaxKind.BindingElement:
      return generateBindingElement(node as BindingElement)
    case SyntaxKind.ArrayLiteralExpression:
      return generateArrayLiteralExpression(node as ArrayLiteralExpression)
    case SyntaxKind.ObjectLiteralExpression:
      return generateObjectLiteralExpression(node as ObjectLiteralExpression)
    case SyntaxKind.PropertyAccessExpression:
      return generatePropertyAccessExpression(node as PropertyAccessExpression)
    case SyntaxKind.ElementAccessExpression:
      return generateElementAccessExpression(node as ElementAccessExpression)
    case SyntaxKind.CallExpression:
      return generateCallExpression(node as CallExpression)
    case SyntaxKind.NewExpression:
      return generateNewExpression(node as NewExpression)
    case SyntaxKind.TaggedTemplateExpression:
      return generateTaggedTemplateExpression(node as TaggedTemplateExpression)
    case SyntaxKind.TypeAssertionExpression:
      return generateTypeAssertionExpression(node as TypeAssertion)
    case SyntaxKind.ParenthesizedExpression:
      return generateParenthesizedExpression(node as ParenthesizedExpression)
    case SyntaxKind.FunctionExpression:
      return generateFunctionExpression(node as FunctionExpression)
    case SyntaxKind.ArrowFunction:
      return generateArrowFunction(node as ArrowFunction)
    case SyntaxKind.DeleteExpression:
      return generateDeleteExpression(node as DeleteExpression)
    case SyntaxKind.TypeOfExpression:
      return generateTypeOfExpression(node as TypeOfExpression)
    case SyntaxKind.VoidExpression:
      return generateVoidExpression(node as VoidExpression)
    case SyntaxKind.AwaitExpression:
      return generateAwaitExpression(node as AwaitExpression)
    case SyntaxKind.BinaryExpression:
      return generateBinaryExpression(node as BinaryExpression)
    case SyntaxKind.ConditionalExpression:
      return generateConditionalExpression(node as ConditionalExpression)
    case SyntaxKind.TemplateExpression:
      return generateTemplateExpression(node as TemplateExpression)
    case SyntaxKind.YieldExpression:
      return generateYieldExpression(node as YieldExpression)
    case SyntaxKind.SpreadElement:
      return generateSpreadElement(node as SpreadElement)
    case SyntaxKind.ClassExpression:
      return generateClassExpression(node as ClassExpression)
    case SyntaxKind.ExpressionWithTypeArguments:
      return generateExpressionWithTypeArguments(node as ExpressionWithTypeArguments)
    case SyntaxKind.AsExpression:
      return generateAsExpression(node as AsExpression)
    case SyntaxKind.NonNullExpression:
      return generateNonNullExpression(node as NonNullExpression)
    case SyntaxKind.MetaProperty:
      return generateMetaProperty(node as MetaProperty)
    case SyntaxKind.TemplateSpan:
      return generateTemplateSpan(node as TemplateSpan)
    case SyntaxKind.SemicolonClassElement:
      return generateSemicolonClassElement(node as SemicolonClassElement)

    case SyntaxKind.Block:
      return generateBlock(node as Block)
    case SyntaxKind.VariableStatement:
      return generateVariableStatement(node as VariableStatement)
    case SyntaxKind.EmptyStatement:
      return generateEmptyStatement(node as EmptyStatement)
    case SyntaxKind.ExpressionStatement:
      return generateExpressionStatement(node as ExpressionStatement)
    case SyntaxKind.IfStatement:
      return generateIfStatement(node as IfStatement)
    case SyntaxKind.DoStatement:
      return generateDoStatement(node as DoStatement)
    case SyntaxKind.WhileStatement:
      return generateWhileStatement(node as WhileStatement)
    case SyntaxKind.ForStatement:
      return generateForStatement(node as ForStatement)
    case SyntaxKind.ForInStatement:
      return generateForInStatement(node as ForInStatement)
    case SyntaxKind.ForOfStatement:
      return generateForOfStatement(node as ForOfStatement)
    case SyntaxKind.ContinueStatement:
      return generateContinueStatement(node as ContinueStatement)
    case SyntaxKind.BreakStatement:
      return generateBreakStatement(node as BreakStatement)
    case SyntaxKind.ReturnStatement:
      return generateReturnStatement(node as ReturnStatement)
    case SyntaxKind.WithStatement:
      return generateWithStatement(node as WithStatement)
    case SyntaxKind.SwitchStatement:
      return generateSwitchStatement(node as SwitchStatement)
    case SyntaxKind.LabeledStatement:
      return generateLabeledStatement(node as LabeledStatement)
    case SyntaxKind.ThrowStatement:
      return generateThrowStatement(node as ThrowStatement)
    case SyntaxKind.TryStatement:
      return generateTryStatement(node as TryStatement)
    case SyntaxKind.DebuggerStatement:
      return generateDebuggerStatement(node as DebuggerStatement)
    case SyntaxKind.VariableDeclaration:
      return generateVariableDeclaration(node as VariableDeclaration)
    case SyntaxKind.VariableDeclarationList:
      return generateVariableDeclarationList(node as VariableDeclarationList)
    case SyntaxKind.FunctionDeclaration:
      return generateFunctionDeclaration(node as FunctionDeclaration)
    case SyntaxKind.ClassDeclaration:
      return generateClassDeclaration(node as ClassDeclaration)
    case SyntaxKind.InterfaceDeclaration:
      return generateInterfaceDeclaration(node as InterfaceDeclaration)
    case SyntaxKind.TypeAliasDeclaration:
      return generateTypeAliasDeclaration(node as TypeAliasDeclaration)
    case SyntaxKind.EnumDeclaration:
      return generateEnumDeclaration(node as EnumDeclaration)
    case SyntaxKind.ModuleDeclaration:
      return generateModuleDeclaration(node as ModuleDeclaration)
    case SyntaxKind.ModuleBlock:
      return generateModuleBlock(node as ModuleBlock)
    case SyntaxKind.CaseBlock:
      return generateCaseBlock(node as CaseBlock)
    case SyntaxKind.NamespaceExportDeclaration:
      return generateNamespaceExportDeclaration(node as NamespaceExportDeclaration)
    case SyntaxKind.ImportEqualsDeclaration:
      return generateImportEqualsDeclaration(node as ImportEqualsDeclaration)
    case SyntaxKind.ImportDeclaration:
      return generateImportDeclaration(node as ImportDeclaration)
    case SyntaxKind.ImportClause:
      return generateImportClause(node as ImportClause)
    case SyntaxKind.NamespaceImport:
      return generateNamespaceImport(node as NamespaceImport)
    case SyntaxKind.NamedImports:
      return generateNamedImports(node as NamedImports)
    case SyntaxKind.ImportSpecifier:
      return generateImportSpecifier(node as ImportSpecifier)
    case SyntaxKind.ExportAssignment:
      return generateExportAssignment(node as ExportAssignment)
    case SyntaxKind.ExportDeclaration:
      return generateExportDeclaration(node as ExportDeclaration)
    case SyntaxKind.NamedExports:
      return generateNamedExports(node as NamedExports)
    case SyntaxKind.ExportSpecifier:
      return generateExportSpecifier(node as ExportSpecifier)
    case SyntaxKind.ExternalModuleReference:
      return generateExternalModuleReference(node as ExternalModuleReference)
    case SyntaxKind.CaseClause:
      return generateCaseClause(node as CaseClause)
    case SyntaxKind.DefaultClause:
      return generateDefaultClause(node as DefaultClause)
    case SyntaxKind.HeritageClause:
      return generateHeritageClause(node as HeritageClause)
    case SyntaxKind.CatchClause:
      return generateCatchClause(node as CatchClause)
    case SyntaxKind.PropertyAssignment:
      return generatePropertyAssignment(node as PropertyAssignment)
    case SyntaxKind.ShorthandPropertyAssignment:
      return generateShorthandPropertyAssignment(node as ShorthandPropertyAssignment)
    case SyntaxKind.SpreadAssignment:
      return generateSpreadAssignment(node as SpreadAssignment)
    case SyntaxKind.EnumMember:
      return generateEnumMember(node as EnumMember)
    case SyntaxKind.PrefixUnaryExpression:
      return generatePrefixUnaryExpression(node as PrefixUnaryExpression)
    case SyntaxKind.PostfixUnaryExpression:
      return generatePostfixUnaryExpression(node as PostfixUnaryExpression)

    case SyntaxKind.SourceFile:
      return generateSourceFile(node as SourceFile)

    case SyntaxKind.JsxElement:
      return generateJsxElement(node as JsxElement)
    case SyntaxKind.JsxSelfClosingElement:
      return generateJsxSelfClosingElement(node as JsxSelfClosingElement)
    case SyntaxKind.JsxOpeningElement:
      return generateJsxOpeningElement(node as JsxOpeningElement)
    case SyntaxKind.JsxClosingElement:
      return generateJsxClosingElement(node as JsxClosingElement)
    case SyntaxKind.JsxAttribute:
      return generateJsxAttribute(node as JsxAttribute)
    case SyntaxKind.JsxAttributes:
      return generateJsxAttributes(node as JsxAttributes)
    case SyntaxKind.JsxSpreadAttribute:
      return generateJsxSpreadAttribute(node as JsxSpreadAttribute)
    case SyntaxKind.JsxExpression:
      return generateJsxExpression(node as JsxExpression)

    case SyntaxKind.JsxFragment:
      return generateJsxFragment(node as JsxFragment)
    case SyntaxKind.JsxOpeningFragment:
      return generateJsxOpeningFragment(node as JsxOpeningFragment)
    case SyntaxKind.JsxClosingFragment:
      return generateJsxClosingFragment(node as JsxClosingFragment)
    case SyntaxKind.JsxText:
      return generateJsxText(node as JsxText)

    case SyntaxKind.MissingDeclaration:
    case SyntaxKind.SyntheticExpression:
    case SyntaxKind.OmittedExpression:
      throw new Error('unknown syntax: ' + node.kind + ' ' + JSON.stringify(node))
    default:
      if (isKeyword(node.kind)) {
        return transformKeyword(node.kind)
      }

      if (isToken(node.kind)) {
        return generateToken(node)
      }

      throw new Error('unsupported syntax: ' + node.kind)
  }
}

export function transformSourceFile(sourceFile: SourceFile): SourceFile {
  return updateSourceFileNode(sourceFile, [createExpressionStatement(transformVisitor(sourceFile))])
}
