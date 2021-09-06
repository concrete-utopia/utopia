import type { MapLike } from 'typescript'
import {
  ControlDescription,
  ObjectControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
  PropertyControls,
  expression,
  importStar,
} from 'utopia-api'

const Vector3: Vector3ControlDescription = {
  type: 'vector3',
}

const Vector2: Vector2ControlDescription = {
  type: 'vector2',
}

const Euler: ObjectControlDescription = {
  type: 'object',
  object: {
    x: {
      type: 'number',
      title: 'x',
    },
    y: {
      type: 'number',
      title: 'y',
    },
    z: {
      type: 'number',
      title: 'z',
    },
    order: {
      type: 'string',
      title: 'order',
    },
  },
}

const nodePropsControls: PropertyControls = {
  attach: {
    type: 'string',
    title: 'attach',
  },
  attachArray: {
    type: 'string',
    title: 'attachArray',
  },
}

const object3DNodePropsControls: PropertyControls = {
  ...nodePropsControls,
  position: Vector3,
  up: Vector3,
  scale: Vector3,
  rotation: Euler,
  // matrix,
  // quarternion,
  // layers,
}

const colorControls: PropertyControls = {
  ...nodePropsControls,
  args: {
    type: 'color',
  },
  attach: {
    type: 'string',
    title: 'attach',
    defaultValue: 'background',
  },
}

const fogControls: PropertyControls = {
  ...nodePropsControls,
  color: {
    type: 'color',
  },
  near: {
    type: 'number',
    defaultValue: 1,
  },
  far: {
    type: 'number',
    defaultValue: 1000,
  },
}

/* LIGHTS */

const lightControls: PropertyControls = {
  ...object3DNodePropsControls,
  color: {
    type: 'string',
    title: 'color',
  },
  intensity: {
    type: 'number',
    title: 'intensity',
    defaultValue: 1,
  },
}

const pointLightControls: PropertyControls = {
  ...lightControls,
  decay: {
    type: 'number',
    defaultValue: 1,
  },
  distance: {
    type: 'number',
    defaultValue: 0,
  },
  power: {
    type: 'number',
    defaultValue: 4 * Math.PI,
  },
  // shadow,
}

const ambientLightControls: PropertyControls = {
  ...lightControls,
}

const directionalLightControls: PropertyControls = {
  ...lightControls,
  castShadow: {
    type: 'boolean',
    defaultValue: false,
  },
  position: Vector3,
  target: Vector3,
}

const spotLightControls: PropertyControls = {
  ...lightControls,
  angle: {
    type: 'number',
    defaultValue: Math.PI / 3,
  },
  castShadow: {
    type: 'boolean',
    defaultValue: false,
  },
  decay: {
    type: 'number',
    defaultValue: 1,
  },
  distance: {
    type: 'number',
    defaultValue: 0,
  },
  penumbra: {
    type: 'number',
    defaultValue: 0,
  },
  power: {
    type: 'number',
    defaultValue: 4 * Math.PI,
  },
  // shadow,
  // target,
}

/* GEOMETRY */

const bufferGeometryControls: PropertyControls = {
  ...nodePropsControls,
  // attributes,
  // boundingBox,
  // boundingSphere,
  // drawRange,
  // groups,
  // id,
  // index,
  // morphAttributes,
  morphTargetsRelative: {
    type: 'boolean',
    defaultValue: false,
  },
  name: {
    type: 'string',
  },
  // userData,
  // uuid
}

const planeGeometryControls: PropertyControls = {
  ...bufferGeometryControls,
  args: {
    type: 'object',
    object: {
      0: {
        type: 'number',
        title: 'width',
      },
      1: {
        type: 'number',
        title: 'height',
      },
      2: {
        type: 'number',
        title: 'widthSegments',
      },
      3: {
        type: 'number',
        title: 'heightSegments',
      },
    },
  },
}

const sphereGeometryControls: PropertyControls = {
  ...bufferGeometryControls,
  args: {
    type: 'object',
    object: {
      0: {
        type: 'number',
        title: 'radius',
        defaultValue: 1,
      },
      1: {
        type: 'number',
        title: 'widthSegments',
        defaultValue: 32,
      },
      2: {
        type: 'number',
        title: 'heightSegments',
        defaultValue: 16,
      },
      3: {
        type: 'number',
        title: 'phiStart',
        defaultValue: 0,
      },
      4: {
        type: 'number',
        title: 'phiLength',
        defaultValue: 2 * Math.PI,
      },
      5: {
        type: 'number',
        title: 'thetaStart',
        defaultValue: 0,
      },
      6: {
        type: 'number',
        title: 'thetaLength',
        defaultValue: Math.PI,
      },
    },
  },
}

const boxGeometryControls: PropertyControls = {
  ...bufferGeometryControls,
  args: Vector3,
}

/* MATERIALS */

const stencilOperations: ControlDescription = {
  type: 'enum',
  options: [0, 7680, 7681, 7682, 7683, 34055, 34056, 5386],
  optionTitles: [
    'ZeroStencilOp',
    'KeepStencilOp',
    'ReplaceStencilOp',
    'IncrementStencilOp',
    'DecrementStencilOp',
    'IncrementWrapStencilOp',
    'DecrementWrapStencilOp',
    'InvertStencilOp',
  ],
  defaultValue: 7680,
}

const materialControls: PropertyControls = {
  ...nodePropsControls,
  alphaTest: {
    type: 'number',
    defaultValue: 0,
  },
  alphaToCoverage: {
    type: 'number',
    defaultValue: 0,
  },
  blendDst: {
    type: 'enum',
    options: [200, 201, 202, 203, 204, 205, 206, 207, 208, 209],
    optionTitles: [
      'ZeroFactor',
      'OneFactor',
      'SrcColorFactor',
      'OneMinusSrcColorFactor',
      'SrcAlphaFactor',
      'OneMinusSrcAlphaFactor',
      'DstAlphaFactor',
      'OneMinusDstAlphaFactor',
      'DstColorFactor',
      'OneMinusDstColorFactor',
    ],
    defaultValue: 205,
  },
  blendDstAlpha: {
    type: 'number',
    defaultValue: null,
  },
  blendEquation: {
    type: 'enum',
    options: [100, 101, 102, 103, 104],
    optionTitles: [
      'AddEquation',
      'SubtractEquation',
      'ReverseSubtractEquation',
      'MinEquation',
      'MaxEquation',
    ],
    defaultValue: 100,
  },
  blendEquationAlpha: {
    type: 'number',
    defaultValue: null,
  },
  blending: {
    type: 'expression-enum',
    optionTitles: [
      'NoBlending',
      'NormalBlending',
      'AdditiveBlending',
      'SubtractiveBlending',
      'MultiplyBlending',
      'CustomBlending',
    ],
    options: [
      expression(0, 'THREE.NoBlending', importStar('three', 'THREE')),
      expression(1, 'THREE.NormalBlending', importStar('three', 'THREE')),
      expression(2, 'THREE.AdditiveBlending', importStar('three', 'THREE')),
      expression(3, 'THREE.SubtractiveBlending', importStar('three', 'THREE')),
      expression(4, 'THREE.MultiplyBlending', importStar('three', 'THREE')),
      expression(5, 'THREE.CustomBlending', importStar('three', 'THREE')),
    ],
    defaultValue: expression(0, 'THREE.NoBlending', importStar('three', 'THREE')),
  },
  blendSrc: {
    type: 'enum',
    options: [200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210],
    optionTitles: [
      'ZeroFactor',
      'OneFactor',
      'SrcColorFactor',
      'OneMinusSrcColorFactor',
      'SrcAlphaFactor',
      'OneMinusSrcAlphaFactor',
      'DstAlphaFactor',
      'OneMinusDstAlphaFactor',
      'DstColorFactor',
      'OneMinusDstColorFactor',
      'SrcAlphaSaturateFactor',
    ],
    defaultValue: 205,
  },
  blendSrcAlpha: {
    type: 'number',
    defaultValue: null,
  },
  clipIntersection: {
    type: 'boolean',
    defaultValue: false,
  },
  // clippingPlanes,
  clipShadows: {
    type: 'boolean',
    defaultValue: false,
  },
  colorWrite: {
    type: 'boolean',
    defaultValue: true,
  },
  // defines,
  depthFunc: {
    type: 'enum',
    options: [0, 1, 4, 2, 3, 5, 6, 7],
    optionTitles: [
      'NeverDepth',
      'AlwaysDepth',
      'EqualDepth',
      'LessDepth',
      'LessEqualDepth',
      'GreaterEqualDepth',
      'GreaterDepth',
      'NotEqualDepth',
    ],
    defaultValue: 3,
  },
  depthTest: {
    type: 'boolean',
    defaultValue: true,
  },
  depthWrite: {
    type: 'boolean',
    defaultValue: true,
  },
  stencilWrite: {
    type: 'boolean',
    defaultValue: false,
  },
  stencilWriteMask: {
    type: 'number',
    defaultValue: 0xff,
  },
  stencilRef: {
    type: 'number',
    defaultValue: 0,
  },
  stencilFuncMask: {
    type: 'number',
    defaultValue: 0xff,
  },
  stencilFail: stencilOperations,
  stencilZFail: stencilOperations,
  stencilZPass: stencilOperations,
  fog: {
    type: 'boolean',
    defaultValue: true,
  },
  opacity: {
    type: 'slider',
    defaultValue: 1.0,
    min: 0.0,
    max: 1.0,
    step: 0.1,
  },
  polygonOffset: {
    type: 'boolean',
    defaultValue: false,
  },
  polygonOffsetFactor: {
    type: 'number',
    defaultValue: 0,
  },
  polygonOffsetUnits: {
    type: 'number',
    defaultValue: 0,
  },
  precision: {
    type: 'enum',
    options: ['highp', 'mediump', 'lowp'],
    defaultValue: null,
  },
  preMultipliedAlpha: {
    type: 'boolean',
    defaultValue: false,
  },
  dithering: {
    type: 'boolean',
    defaultValue: false,
  },
  shadowSide: {
    type: 'enum',
    options: [0, 1, 2],
    optionTitles: ['FrontSide', 'BackSide', 'DoubleSide'],
    defaultValue: null,
  },
  side: {
    type: 'enum',
    options: [0, 1, 2],
    optionTitles: ['FrontSide', 'BackSide', 'DoubleSide'],
    defaultValue: 0,
  },
  toneMapped: {
    type: 'boolean',
    defaultValue: true,
  },
  transparent: {
    type: 'boolean',
    defaultValue: false,
  },
  vertexColors: {
    type: 'boolean',
    defaultValue: false,
  },
  visible: {
    type: 'boolean',
    defaultValue: true,
  },
}

const meshBasicMaterialControls: PropertyControls = {
  ...materialControls,
  // alphaMap,
  // aoMap,
  aoMapIntensity: {
    type: 'number',
    defaultValue: 1,
  },
  color: {
    type: 'color',
  },
  combine: {
    type: 'enum',
    options: [0, 1, 2],
    optionTitles: ['Multiply', 'MixOperation', 'AddOperation'],
    defaultValue: 0,
  },
  // envMap,
  // lightMap,
  lightMapIntensity: {
    type: 'number',
    defaultValue: 1,
  },
  // map,
  reflectivity: {
    type: 'slider',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 1,
  },
  refractionRatio: {
    type: 'slider',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 0.98,
  },
  // specularMap,
  wireframe: {
    type: 'boolean',
    defaultValue: false,
  },
  wireframeLinecap: {
    type: 'enum',
    options: ['butt', 'round', 'square'],
    defaultValue: 'round',
  },
  wireframeLinejoin: {
    type: 'enum',
    options: ['round', 'bevel', 'miter'],
    defaultValue: 'round',
  },
  wireframeLinewidth: {
    type: 'number',
    defaultValue: 1,
  },
}

const meshStandardMaterialControls: PropertyControls = {
  ...materialControls,
  // alphaMap,
  // aoMap,
  aoMapIntensity: {
    type: 'number',
    defaultValue: 1,
  },
  // bumpMap,
  bumpScale: {
    type: 'slider',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 1,
  },
  color: {
    type: 'color',
  },
  // defines,
  // displacementMap,
  displacementScale: {
    type: 'number',
    defaultValue: 1,
  },
  displacementBias: {
    type: 'number',
    defaultValue: 0,
  },
  emissive: {
    type: 'color',
  },
  // emissiveMap,
  emissiveIntensity: {
    type: 'number',
    defaultValue: 1,
  },
  // envMap,
  envMapIntensity: {
    type: 'number',
  },
  flatShading: {
    type: 'boolean',
    defaultValue: false,
  },
  // lightMap,
  lightMapIntensity: {
    type: 'number',
    defaultValue: 1,
  },
  // map,
  metalness: {
    type: 'slider',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 0,
  },
  // metalnessMap,
  // normalMap,
  // normalMapType,
  // normalScale,
  refractionRatio: {
    type: 'slider',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 0.98,
  },
  roughness: {
    type: 'slider',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 1,
  },
  // roughnessMap,
  wireframe: {
    type: 'boolean',
    defaultValue: false,
  },
  wireframeLinecap: {
    type: 'enum',
    options: ['butt', 'round', 'square'],
    defaultValue: 'round',
  },
  wireframeLinejoin: {
    type: 'enum',
    options: ['round', 'bevel', 'miter'],
    defaultValue: 'round',
  },
  wireframeLinewidth: {
    type: 'number',
    defaultValue: 1,
  },
}

const shadowMaterialControls: PropertyControls = {
  ...materialControls,
  transparent: {
    type: 'boolean',
    defaultValue: true,
  },
}

export const ReactThreeFiberControls: MapLike<PropertyControls> = {
  color: colorControls,
  fog: fogControls,
  ambientLight: ambientLightControls,
  directionalLight: directionalLightControls,
  pointLight: pointLightControls,
  spotLight: spotLightControls,
  boxGeometry: boxGeometryControls,
  planeGeometry: planeGeometryControls,
  sphereGeometry: sphereGeometryControls,
  meshBasicMaterial: meshBasicMaterialControls,
  meshStandardMaterial: meshStandardMaterialControls,
  shadowMaterial: shadowMaterialControls,
}
