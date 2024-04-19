import type { PropertyControls } from '../../components/custom-code/internal-property-controls'
import { ReactThreeFiberControls } from '../property-controls/third-party-property-controls/react-three-fiber-controls'
import {
  defaultComponentDescriptor,
  type ComponentDescriptor,
  type ComponentDescriptorsForFile,
  ComponentDescriptorDefaults,
} from '../../components/custom-code/code-file'
import type { JSXAttributes, JSXAttributesEntry } from '../shared/element-template'
import {
  emptyComments,
  jsOpaqueArbitraryStatement,
  jsExpressionOtherJavaScript,
  jsxAttributesEntry,
  jsxElementWithoutUID,
  simpleAttribute,
} from '../shared/element-template'
import type { Imports } from '../shared/project-file-types'
import { importDetails } from '../shared/project-file-types'

function threeAttribute(key: string, fromThree: string): JSXAttributesEntry {
  return jsxAttributesEntry(
    key,
    jsExpressionOtherJavaScript(
      [],
      `THREE.${fromThree}`,
      `THREE.${fromThree}`,
      `return THREE.${fromThree}`,
      ['THREE'],
      null,
      {},
      emptyComments,
    ),
    emptyComments,
  )
}

function createBasicComponent(
  name: string,
  propertyControls: PropertyControls,
  attributes?: () => JSXAttributes,
  importsToAdd?: Imports,
): ComponentDescriptor {
  return {
    properties: propertyControls,
    supportsChildren: false,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: name,
        importsToAdd: importsToAdd ?? {},
        elementToInsert: () => jsxElementWithoutUID(name, attributes?.() ?? [], []),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  }
}

const materialDefaultValues: JSXAttributes = [
  simpleAttribute('alphaTest', 0),
  simpleAttribute('alphaToCoverage', 0),
  threeAttribute('blendDst', 'OneMinusSrcAlphaFactor'),
  simpleAttribute('blendDstAlpha', null),
  threeAttribute('blendEquation', 'AddEquation'),
  simpleAttribute('blendEquationAlpha', null),
  threeAttribute('blending', 'NoBlending'),
  threeAttribute('blendSrc', 'OneMinusSrcAlphaFactor'),
  simpleAttribute('blendSrcAlpha', null),
  simpleAttribute('clipIntersection', false),
  simpleAttribute('clipShadows', false),
  simpleAttribute('colorWrite', true),
  threeAttribute('depthFunc', 'LessEqualDepth'),
  simpleAttribute('depthTest', true),
  simpleAttribute('depthWrite', true),
  simpleAttribute('stencilWrite', false),
  simpleAttribute('stencilWriteMask', 0xff),
  simpleAttribute('stencilRef', 0),
  simpleAttribute('stencilFuncMask', 0xff),
  threeAttribute('stencilFail', 'KeepStencilOp'),
  threeAttribute('stencilZFail', 'KeepStencilOp'),
  threeAttribute('stencilZPass', 'KeepStencilOp'),
  simpleAttribute('fog', true),
  simpleAttribute('opacity', 1.0),
  simpleAttribute('polygonOffset', false),
  simpleAttribute('polygonOffsetFactor', 0),
  simpleAttribute('polygonOffsetUnits', 0),
  simpleAttribute('precision', null),
  simpleAttribute('preMultipliedAlpha', false),
  simpleAttribute('dithering', false),
  threeAttribute('shadowSide', 'FrontSide'),
  threeAttribute('side', 'FrontSide'),
  simpleAttribute('toneMapped', true),
  simpleAttribute('transparent', false),
  simpleAttribute('vertexColors', false),
  simpleAttribute('visible', true),
]

export const ReactThreeFiberComponents: ComponentDescriptorsForFile = {
  color: createBasicComponent('color', ReactThreeFiberControls.color, () => [
    simpleAttribute('attach', 1),
  ]),
  fog: createBasicComponent('fog', ReactThreeFiberControls.fog, () => [
    simpleAttribute('near', 1),
    simpleAttribute('far', 1000),
  ]),
  ambientLight: createBasicComponent('ambientLight', ReactThreeFiberControls.ambientLight, () => [
    simpleAttribute('intensity', 1),
  ]),
  directionalLight: createBasicComponent(
    'directionalLight',
    ReactThreeFiberControls.directionalLight,
    () => [simpleAttribute('intensity', 1), simpleAttribute('castShadow', false)],
  ),
  pointLight: createBasicComponent('pointLight', ReactThreeFiberControls.pointLight, () => [
    simpleAttribute('intensity', 1),
    simpleAttribute('decay', 1),
    simpleAttribute('distance', 0),
    simpleAttribute('power', 4 * Math.PI),
  ]),
  spotLight: createBasicComponent('spotLight', ReactThreeFiberControls.spotLight, () => [
    simpleAttribute('intensity', 1),
    simpleAttribute('angle', Math.PI / 3),
    simpleAttribute('castShadow', false),
    simpleAttribute('decay', 1),
    simpleAttribute('distance', 0),
    simpleAttribute('penumbra', 0),
    simpleAttribute('power', 4 * Math.PI),
  ]),
  boxGeometry: createBasicComponent('boxGeometry', ReactThreeFiberControls.boxGeometry, () => [
    simpleAttribute('morphTargetsRelative', false),
  ]),
  planeGeometry: createBasicComponent(
    'planeGeometry',
    ReactThreeFiberControls.planeGeometry,
    () => [simpleAttribute('morphTargetsRelative', false)],
  ),
  sphereGeometry: createBasicComponent(
    'sphereGeometry',
    ReactThreeFiberControls.sphereGeometry,
    () => [
      simpleAttribute('morphTargetsRelative', false),
      simpleAttribute('args', [1, 32, 16, 0, 2 * Math.PI, 0, Math.PI]),
    ],
  ),
  meshBasicMaterial: createBasicComponent(
    'meshBasicMaterial',
    ReactThreeFiberControls.meshBasicMaterial,
    () => [
      ...materialDefaultValues,
      simpleAttribute('aoMapIntensity', 1),
      threeAttribute('combine', 'Multiply'),
      simpleAttribute('lightMapIntensity', 1),
      simpleAttribute('reflectivity', 1),
      simpleAttribute('refractionRatio', 0.98),
      simpleAttribute('wireframe', false),
      simpleAttribute('wireframeLinecap', 'round'),
      simpleAttribute('wireframeLinejoin', 'round'),
      simpleAttribute('wireframeLinewidth', 1),
    ],
    { three: importDetails(null, [], 'THREE') },
  ),
  meshStandardMaterial: createBasicComponent(
    'meshStandardMaterial',
    ReactThreeFiberControls.meshStandardMaterial,
    () => [
      ...materialDefaultValues,
      simpleAttribute('aoMapIntensity', 1),
      simpleAttribute('bumpScale', 1),
      simpleAttribute('displacementScale', 1),
      simpleAttribute('displacementBias', 0),
      simpleAttribute('emissiveIntensity', 1),
      simpleAttribute('flatShading', false),
      simpleAttribute('lightMapIntensity', 1),
      simpleAttribute('metalness', 0),
      simpleAttribute('refractionRatio', 0.98),
      simpleAttribute('roughness', 1),
      simpleAttribute('wireframe', false),
      simpleAttribute('wireframeLinecap', 'round'),
      simpleAttribute('wireframeLinejoin', 'round'),
      simpleAttribute('wireframeLinewidth', 1),
    ],
    { three: importDetails(null, [], 'THREE') },
  ),
  shadowMaterial: createBasicComponent(
    'shadowMaterial',
    ReactThreeFiberControls.shadowMaterial,
    () => [...materialDefaultValues, simpleAttribute('transparent', true)],
    { three: importDetails(null, [], 'THREE') },
  ),
}
