import type { MapLike } from 'typescript'
import { ObjectControlDescription, PropertyControls } from 'utopia-api'

const Vector3: ObjectControlDescription = {
  type: 'object',
  object: {
    0: {
      type: 'number',
      title: 'x',
    },
    1: {
      type: 'number',
      title: 'y',
    },
    2: {
      type: 'number',
      title: 'z',
    },
  },
}

const lightControls: PropertyControls = {
  position: Vector3,
  rotation: Vector3,
  color: {
    type: 'string',
    title: 'color',
  },
  intensity: {
    type: 'number',
    title: 'intensity',
    defaultValue: 1,
  },
  distance: {
    type: 'number',
    title: 'distance',
    defaultValue: 0,
  },
  power: {
    type: 'number',
    title: 'power',
  },
}

const sphereGeometryControls: PropertyControls = {
  args: Vector3,
  attach: {
    type: 'string',
    title: 'attach',
    defaultValue: 'geometry',
  },
}

const boxGeometryControls: PropertyControls = {
  args: Vector3,
}

const meshStandardMaterialControls: PropertyControls = {
  attach: {
    type: 'string',
    title: 'attach',
    defaultValue: 'material',
  },
  color: {
    type: 'string',
    title: 'color',
  },
  transparent: {
    type: 'boolean',
    title: 'transparent',
  },
}

export const ReactThreeFiberControls: MapLike<PropertyControls> = {
  pointLight: lightControls,
  directionalLight: lightControls,
  ambientLight: lightControls,
  sphereGeometry: sphereGeometryControls,
  meshStandardMaterial: meshStandardMaterialControls,
  boxGeometry: boxGeometryControls,
}
