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
  control: 'vector3',
}

const Vector2: Vector2ControlDescription = {
  control: 'vector2',
}

const Euler: ObjectControlDescription = {
  control: 'object',
  object: {
    x: {
      control: 'number-input',
      label: 'x',
    },
    y: {
      control: 'number-input',
      label: 'y',
    },
    z: {
      control: 'number-input',
      label: 'z',
    },
    order: {
      control: 'string-input',
      label: 'order',
    },
  },
}

const nodePropsControls: PropertyControls = {
  attach: {
    control: 'string-input',
    label: 'attach',
  },
  attachArray: {
    control: 'string-input',
    label: 'attachArray',
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
    control: 'color',
  },
  attach: {
    control: 'string-input',
    label: 'attach',
    defaultValue: 'background',
  },
}

const fogControls: PropertyControls = {
  ...nodePropsControls,
  color: {
    control: 'color',
  },
  near: {
    control: 'number-input',
    defaultValue: 1,
  },
  far: {
    control: 'number-input',
    defaultValue: 1000,
  },
}

/* LIGHTS */

const lightControls: PropertyControls = {
  ...object3DNodePropsControls,
  color: {
    control: 'string-input',
    label: 'color',
  },
  intensity: {
    control: 'number-input',
    label: 'intensity',
    defaultValue: 1,
  },
}

const pointLightControls: PropertyControls = {
  ...lightControls,
  decay: {
    control: 'number-input',
    defaultValue: 1,
  },
  distance: {
    control: 'number-input',
    defaultValue: 0,
  },
  power: {
    control: 'number-input',
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
    control: 'checkbox',
    defaultValue: false,
  },
  target: Vector3,
}

const spotLightControls: PropertyControls = {
  ...lightControls,
  angle: {
    control: 'number-input',
    defaultValue: Math.PI / 3,
  },
  castShadow: {
    control: 'checkbox',
    defaultValue: false,
  },
  decay: {
    control: 'number-input',
    defaultValue: 1,
  },
  distance: {
    control: 'number-input',
    defaultValue: 0,
  },
  penumbra: {
    control: 'number-input',
    defaultValue: 0,
  },
  power: {
    control: 'number-input',
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
    control: 'checkbox',
    defaultValue: false,
  },
  name: {
    control: 'string-input',
  },
  // userData,
  // uuid
}

const planeGeometryControls: PropertyControls = {
  ...bufferGeometryControls,
  args: {
    control: 'object',
    object: {
      0: {
        control: 'number-input',
        label: 'width',
      },
      1: {
        control: 'number-input',
        label: 'height',
      },
      2: {
        control: 'number-input',
        label: 'widthSegments',
      },
      3: {
        control: 'number-input',
        label: 'heightSegments',
      },
    },
  },
}

const sphereGeometryControls: PropertyControls = {
  ...bufferGeometryControls,
  args: {
    control: 'object',
    object: {
      0: {
        control: 'number-input',
        label: 'radius',
        defaultValue: 1,
      },
      1: {
        control: 'number-input',
        label: 'widthSegments',
        defaultValue: 32,
      },
      2: {
        control: 'number-input',
        label: 'heightSegments',
        defaultValue: 16,
      },
      3: {
        control: 'number-input',
        label: 'phiStart',
        defaultValue: 0,
      },
      4: {
        control: 'number-input',
        label: 'phiLength',
        defaultValue: 2 * Math.PI,
      },
      5: {
        control: 'number-input',
        label: 'thetaStart',
        defaultValue: 0,
      },
      6: {
        control: 'number-input',
        label: 'thetaLength',
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
  control: 'expression-popuplist',
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
    control: 'number-input',
    defaultValue: 0,
  },
  alphaToCoverage: {
    control: 'number-input',
    defaultValue: 0,
  },
  Blending: {
    control: 'folder',
    controls: {
      blendDst: {
        control: 'expression-popuplist',
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
        control: 'number-input',
        defaultValue: null,
      },
      blendEquation: {
        control: 'expression-popuplist',
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
        control: 'number-input',
        defaultValue: null,
      },
      blending: {
        control: 'expression-popuplist',
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
        control: 'expression-popuplist',
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
        control: 'number-input',
        defaultValue: null,
      },
    },
  },
  clipIntersection: {
    control: 'checkbox',
    defaultValue: false,
  },
  // clippingPlanes,
  clipShadows: {
    control: 'checkbox',
    defaultValue: false,
  },
  colorWrite: {
    control: 'checkbox',
    defaultValue: true,
  },
  // defines,
  depthFunc: {
    control: 'expression-popuplist',
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
    control: 'checkbox',
    defaultValue: true,
  },
  depthWrite: {
    control: 'checkbox',
    defaultValue: true,
  },
  stencilWrite: {
    control: 'checkbox',
    defaultValue: false,
  },
  stencilWriteMask: {
    control: 'number-input',
    defaultValue: 0xff,
  },
  stencilRef: {
    control: 'number-input',
    defaultValue: 0,
  },
  stencilFuncMask: {
    control: 'number-input',
    defaultValue: 0xff,
  },
  stencilFail: stencilOperations,
  stencilZFail: stencilOperations,
  stencilZPass: stencilOperations,
  fog: {
    control: 'checkbox',
    defaultValue: true,
  },
  opacity: {
    control: 'number-input',
    defaultValue: 1.0,
    min: 0.0,
    max: 1.0,
    step: 0.1,
  },
  polygonOffset: {
    control: 'checkbox',
    defaultValue: false,
  },
  polygonOffsetFactor: {
    control: 'number-input',
    defaultValue: 0,
  },
  polygonOffsetUnits: {
    control: 'number-input',
    defaultValue: 0,
  },
  precision: {
    control: 'popuplist',
    options: ['highp', 'mediump', 'lowp'],
    defaultValue: null,
  },
  preMultipliedAlpha: {
    control: 'checkbox',
    defaultValue: false,
  },
  dithering: {
    control: 'checkbox',
    defaultValue: false,
  },
  shadowSide: {
    control: 'expression-popuplist',
    options: [
      expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
      expression(1, 'THREE.BackSide', importStar('three', 'THREE')),
      expression(2, 'THREE.DoubleSide', importStar('three', 'THREE')),
    ],
    optionTitles: ['FrontSide', 'BackSide', 'DoubleSide'],
    defaultValue: expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
  },
  side: {
    control: 'expression-popuplist',
    options: [
      expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
      expression(1, 'THREE.BackSide', importStar('three', 'THREE')),
      expression(2, 'THREE.DoubleSide', importStar('three', 'THREE')),
    ],
    optionTitles: ['FrontSide', 'BackSide', 'DoubleSide'],
    defaultValue: expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
  },
  toneMapped: {
    control: 'checkbox',
    defaultValue: true,
  },
  transparent: {
    control: 'checkbox',
    defaultValue: false,
  },
  vertexColors: {
    control: 'checkbox',
    defaultValue: false,
  },
  visible: {
    control: 'checkbox',
    defaultValue: true,
  },
}

const meshBasicMaterialControls: PropertyControls = {
  ...materialControls,
  // alphaMap,
  // aoMap,
  aoMapIntensity: {
    control: 'number-input',
    defaultValue: 1,
  },
  color: {
    control: 'color',
  },
  combine: {
    control: 'expression-popuplist',
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
    control: 'number-input',
    defaultValue: 1,
  },
  // map,
  reflectivity: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 1,
  },
  refractionRatio: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 0.98,
  },
  // specularMap,
  wireframe: {
    control: 'checkbox',
    defaultValue: false,
  },
  wireframeLinecap: {
    control: 'popuplist',
    options: ['butt', 'round', 'square'],
    defaultValue: 'round',
  },
  wireframeLinejoin: {
    control: 'popuplist',
    options: ['round', 'bevel', 'miter'],
    defaultValue: 'round',
  },
  wireframeLinewidth: {
    control: 'number-input',
    defaultValue: 1,
  },
}

const meshStandardMaterialControls: PropertyControls = {
  ...materialControls,
  // alphaMap,
  // aoMap,
  aoMapIntensity: {
    control: 'number-input',
    defaultValue: 1,
  },
  // bumpMap,
  bumpScale: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 1,
  },
  color: {
    control: 'color',
  },
  // defines,
  // displacementMap,
  displacementScale: {
    control: 'number-input',
    defaultValue: 1,
  },
  displacementBias: {
    control: 'number-input',
    defaultValue: 0,
  },
  emissive: {
    control: 'color',
  },
  // emissiveMap,
  emissiveIntensity: {
    control: 'number-input',
    defaultValue: 1,
  },
  // envMap,
  envMapIntensity: {
    control: 'number-input',
  },
  flatShading: {
    control: 'checkbox',
    defaultValue: false,
  },
  // lightMap,
  lightMapIntensity: {
    control: 'number-input',
    defaultValue: 1,
  },
  // map,
  metalness: {
    control: 'number-input',
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
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 0.98,
  },
  roughness: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
    defaultValue: 1,
  },
  // roughnessMap,
  wireframe: {
    control: 'checkbox',
    defaultValue: false,
  },
  wireframeLinecap: {
    control: 'popuplist',
    options: ['butt', 'round', 'square'],
    defaultValue: 'round',
  },
  wireframeLinejoin: {
    control: 'popuplist',
    options: ['round', 'bevel', 'miter'],
    defaultValue: 'round',
  },
  wireframeLinewidth: {
    control: 'number-input',
    defaultValue: 1,
  },
}

const shadowMaterialControls: PropertyControls = {
  ...materialControls,
  transparent: {
    control: 'checkbox',
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
