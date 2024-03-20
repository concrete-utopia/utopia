import { expression, importStar } from 'utopia-api/core'
import type {
  ObjectControlDescription,
  Vector2ControlDescription,
  ControlDescription,
  PropertyControls,
  Vector3ControlDescription,
} from '../../../components/custom-code/internal-property-controls'

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
  },
}

const fogControls: PropertyControls = {
  ...nodePropsControls,
  color: {
    control: 'color',
  },
  near: {
    control: 'number-input',
  },
  far: {
    control: 'number-input',
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
  },
}

const pointLightControls: PropertyControls = {
  ...lightControls,
  decay: {
    control: 'number-input',
  },
  distance: {
    control: 'number-input',
  },
  power: {
    control: 'number-input',
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
  },
  target: Vector3,
}

const spotLightControls: PropertyControls = {
  ...lightControls,
  angle: {
    control: 'number-input',
  },
  castShadow: {
    control: 'checkbox',
  },
  decay: {
    control: 'number-input',
  },
  distance: {
    control: 'number-input',
  },
  penumbra: {
    control: 'number-input',
  },
  power: {
    control: 'number-input',
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
      },
      1: {
        control: 'number-input',
        label: 'widthSegments',
      },
      2: {
        control: 'number-input',
        label: 'heightSegments',
      },
      3: {
        control: 'number-input',
        label: 'phiStart',
      },
      4: {
        control: 'number-input',
        label: 'phiLength',
      },
      5: {
        control: 'number-input',
        label: 'thetaStart',
      },
      6: {
        control: 'number-input',
        label: 'thetaLength',
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
}

const materialControls: PropertyControls = {
  ...nodePropsControls,
  alphaTest: {
    control: 'number-input',
  },
  alphaToCoverage: {
    control: 'number-input',
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
      },
      blendDstAlpha: {
        control: 'number-input',
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
      },
      blendEquationAlpha: {
        control: 'number-input',
      },
      blending: {
        control: 'expression-popuplist',
        options: [
          expression(0, 'THREE.NoBlending', importStar('three', 'THREE')),
          expression(1, 'THREE.NormalBlending', importStar('three', 'THREE')),
          expression(2, 'THREE.AdditiveBlending', importStar('three', 'THREE')),
          expression(3, 'THREE.SubtractiveBlending', importStar('three', 'THREE')),
          expression(4, 'THREE.MultiplyBlending', importStar('three', 'THREE')),
          expression(5, 'THREE.CustomBlending', importStar('three', 'THREE')),
        ],
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
      },
      blendSrcAlpha: {
        control: 'number-input',
      },
    },
  },
  clipIntersection: {
    control: 'checkbox',
  },
  // clippingPlanes,
  clipShadows: {
    control: 'checkbox',
  },
  colorWrite: {
    control: 'checkbox',
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
  },
  depthTest: {
    control: 'checkbox',
  },
  depthWrite: {
    control: 'checkbox',
  },
  stencilWrite: {
    control: 'checkbox',
  },
  stencilWriteMask: {
    control: 'number-input',
  },
  stencilRef: {
    control: 'number-input',
  },
  stencilFuncMask: {
    control: 'number-input',
  },
  stencilFail: stencilOperations,
  stencilZFail: stencilOperations,
  stencilZPass: stencilOperations,
  fog: {
    control: 'checkbox',
  },
  opacity: {
    control: 'number-input',
    min: 0.0,
    max: 1.0,
    step: 0.1,
  },
  polygonOffset: {
    control: 'checkbox',
  },
  polygonOffsetFactor: {
    control: 'number-input',
  },
  polygonOffsetUnits: {
    control: 'number-input',
  },
  precision: {
    control: 'popuplist',
    options: ['highp', 'mediump', 'lowp'],
  },
  preMultipliedAlpha: {
    control: 'checkbox',
  },
  dithering: {
    control: 'checkbox',
  },
  shadowSide: {
    control: 'expression-popuplist',
    options: [
      expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
      expression(1, 'THREE.BackSide', importStar('three', 'THREE')),
      expression(2, 'THREE.DoubleSide', importStar('three', 'THREE')),
    ],
  },
  side: {
    control: 'expression-popuplist',
    options: [
      expression(0, 'THREE.FrontSide', importStar('three', 'THREE')),
      expression(1, 'THREE.BackSide', importStar('three', 'THREE')),
      expression(2, 'THREE.DoubleSide', importStar('three', 'THREE')),
    ],
  },
  toneMapped: {
    control: 'checkbox',
  },
  transparent: {
    control: 'checkbox',
  },
  vertexColors: {
    control: 'checkbox',
  },
  visible: {
    control: 'checkbox',
  },
}

const meshBasicMaterialControls: PropertyControls = {
  ...materialControls,
  // alphaMap,
  // aoMap,
  aoMapIntensity: {
    control: 'number-input',
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
  },
  // envMap,
  // lightMap,
  lightMapIntensity: {
    control: 'number-input',
  },
  // map,
  reflectivity: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
  },
  refractionRatio: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
  },
  // specularMap,
  wireframe: {
    control: 'checkbox',
  },
  wireframeLinecap: {
    control: 'popuplist',
    options: ['butt', 'round', 'square'],
  },
  wireframeLinejoin: {
    control: 'popuplist',
    options: ['round', 'bevel', 'miter'],
  },
  wireframeLinewidth: {
    control: 'number-input',
  },
}

const meshStandardMaterialControls: PropertyControls = {
  ...materialControls,
  // alphaMap,
  // aoMap,
  aoMapIntensity: {
    control: 'number-input',
  },
  // bumpMap,
  bumpScale: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
  },
  color: {
    control: 'color',
  },
  // defines,
  // displacementMap,
  displacementScale: {
    control: 'number-input',
  },
  displacementBias: {
    control: 'number-input',
  },
  emissive: {
    control: 'color',
  },
  // emissiveMap,
  emissiveIntensity: {
    control: 'number-input',
  },
  // envMap,
  envMapIntensity: {
    control: 'number-input',
  },
  flatShading: {
    control: 'checkbox',
  },
  // lightMap,
  lightMapIntensity: {
    control: 'number-input',
  },
  // map,
  metalness: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
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
  },
  roughness: {
    control: 'number-input',
    min: 0,
    max: 1,
    step: 0.05,
  },
  // roughnessMap,
  wireframe: {
    control: 'checkbox',
  },
  wireframeLinecap: {
    control: 'popuplist',
    options: ['butt', 'round', 'square'],
  },
  wireframeLinejoin: {
    control: 'popuplist',
    options: ['round', 'bevel', 'miter'],
  },
  wireframeLinewidth: {
    control: 'number-input',
  },
}

const shadowMaterialControls: PropertyControls = {
  ...materialControls,
  transparent: {
    control: 'checkbox',
  },
}

export const ReactThreeFiberControls = {
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
