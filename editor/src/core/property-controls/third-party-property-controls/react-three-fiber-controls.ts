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
  type: 'expression-enum',
  options: [
    expression(0, 'THREE.ZeroStencilOp', importStar('three', 'THREE')),
    expression(7680, 'THREE.KeepStencilOp', importStar('three', 'THREE')),
    expression(7681, 'THREE.ReplaceStencilOp', importStar('three', 'THREE')),
    expression(7682, 'THREE.IncrementStencilOp', importStar('three', 'THREE')),
    expression(7683, 'THREE.DecrementStencilOp', importStar('three', 'THREE')),
    expression(34055, 'THREE.IncrementWrapStencilOp', importStar('three', 'THREE')),
    expression(34056, 'THREE.DecrementWrapStencilOp', importStar('three', 'THREE')),
    expression(5386, 'THREE.InvertStencilOp', importStar('three', 'THREE')),
  ],
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
  defaultValue: expression(7680, 'THREE.KeepStencilOp', importStar('three', 'THREE')),
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
    type: 'expression-enum',
    options: [
      expression(200, 'THREE.ZeroFactor', importStar('three', 'THREE')),
      expression(201, 'THREE.OneFactor', importStar('three', 'THREE')),
      expression(202, 'THREE.SrcColorFactor', importStar('three', 'THREE')),
      expression(203, 'THREE.OneMinusSrcColorFactor', importStar('three', 'THREE')),
      expression(204, 'THREE.SrcAlphaFactor', importStar('three', 'THREE')),
      expression(205, 'THREE.OneMinusSrcAlphaFactor', importStar('three', 'THREE')),
      expression(206, 'THREE.DstAlphaFactor', importStar('three', 'THREE')),
      expression(207, 'THREE.OneMinusDstAlphaFactor', importStar('three', 'THREE')),
      expression(208, 'THREE.DstColorFactor', importStar('three', 'THREE')),
      expression(209, 'THREE.OneMinusDstColorFactor', importStar('three', 'THREE')),
    ],
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
    defaultValue: expression(205, 'THREE.OneMinusSrcAlphaFactor', importStar('three', 'THREE')),
  },
  blendDstAlpha: {
    type: 'number',
    defaultValue: null,
  },
  blendEquation: {
    type: 'expression-enum',
    options: [
      expression(100, 'THREE.AddEquation', importStar('three', 'THREE')),
      expression(101, 'THREE.SubtractEquation', importStar('three', 'THREE')),
      expression(102, 'THREE.ReverseSubtractEquation', importStar('three', 'THREE')),
      expression(103, 'THREE.MinEquation', importStar('three', 'THREE')),
      expression(104, 'THREE.MaxEquation', importStar('three', 'THREE')),
    ],
    optionTitles: [
      'AddEquation',
      'SubtractEquation',
      'ReverseSubtractEquation',
      'MinEquation',
      'MaxEquation',
    ],
    defaultValue: expression(100, 'THREE.AddEquation', importStar('three', 'THREE')),
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
    type: 'expression-enum',
    options: [
      expression(200, 'THREE.ZeroFactor', importStar('three', 'THREE')),
      expression(201, 'THREE.OneFactor', importStar('three', 'THREE')),
      expression(202, 'THREE.SrcColorFactor', importStar('three', 'THREE')),
      expression(203, 'THREE.OneMinusSrcColorFactor', importStar('three', 'THREE')),
      expression(204, 'THREE.SrcAlphaFactor', importStar('three', 'THREE')),
      expression(205, 'THREE.OneMinusSrcAlphaFactor', importStar('three', 'THREE')),
      expression(206, 'THREE.DstAlphaFactor', importStar('three', 'THREE')),
      expression(207, 'THREE.OneMinusDstAlphaFactor', importStar('three', 'THREE')),
      expression(208, 'THREE.DstColorFactor', importStar('three', 'THREE')),
      expression(209, 'THREE.OneMinusDstColorFactor', importStar('three', 'THREE')),
      expression(210, 'THREE.SrcAlphaSaturateFactor', importStar('three', 'THREE')),
    ],
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
    defaultValue: expression(205, 'THREE.OneMinusSrcAlphaFactor', importStar('three', 'THREE')),
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
    type: 'expression-enum',
    options: [
      expression(0, 'THREE.NeverDepth', importStar('three', 'THREE')),
      expression(1, 'THREE.AlwaysDepth', importStar('three', 'THREE')),
      expression(4, 'THREE.EqualDepth', importStar('three', 'THREE')),
      expression(2, 'THREE.LessDepth', importStar('three', 'THREE')),
      expression(3, 'THREE.LessEqualDepth', importStar('three', 'THREE')),
      expression(5, 'THREE.GreaterEqualDepth', importStar('three', 'THREE')),
      expression(6, 'THREE.GreaterDepth', importStar('three', 'THREE')),
      expression(7, 'THREE.NotEqualDepth', importStar('three', 'THREE')),
    ],
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
    defaultValue: expression(3, 'THREE.LessEqualDepth', importStar('three', 'THREE')),
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
    type: 'expression-enum',
    options: [
      expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
      expression(1, 'THREE.BackSide', importStar('three', 'THREE')),
      expression(2, 'THREE.DoubleSide', importStar('three', 'THREE')),
    ],
    optionTitles: ['FrontSide', 'BackSide', 'DoubleSide'],
    defaultValue: expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
  },
  side: {
    type: 'expression-enum',
    options: [
      expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
      expression(1, 'THREE.BackSide', importStar('three', 'THREE')),
      expression(2, 'THREE.DoubleSide', importStar('three', 'THREE')),
    ],
    optionTitles: ['FrontSide', 'BackSide', 'DoubleSide'],
    defaultValue: expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
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
    type: 'expression-enum',
    options: [
      expression(0, 'THREE.Multiply', importStar('three', 'THREE')),
      expression(1, 'THREE.MixOperation', importStar('three', 'THREE')),
      expression(2, 'THREE.AddOperation', importStar('three', 'THREE')),
    ],
    optionTitles: ['Multiply', 'MixOperation', 'AddOperation'],
    defaultValue: expression(0, 'THREE.Multiply', importStar('three', 'THREE')),
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
