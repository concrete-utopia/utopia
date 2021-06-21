import type { MapLike } from 'typescript'
import { PropertyControls } from 'utopia-api'

const lightControls: PropertyControls = {
  position: {
    type: 'array',
    propertyControl: {
      type: 'number',
      title: 'position',
    },
    maxCount: 3,
  },
  rotation: {
    type: 'array',
    propertyControl: {
      type: 'number',
      title: 'rotation',
    },
    maxCount: 3,
  },
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
  args: {
    type: 'array',
    propertyControl: {
      type: 'number',
      title: 'args',
    },
    maxCount: 3,
  },
  attach: {
    type: 'string',
    title: 'attach',
    defaultValue: 'geometry',
  },
}

const boxGeometryControls: PropertyControls = {
  args: {
    type: 'array',
    propertyControl: {
      type: 'number',
      title: 'args',
    },
    maxCount: 3,
  },
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
