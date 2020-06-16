export type FeatureName = 'Dragging Reparents By Default' | 'Dragging Shows Overlay'
export const AllFeatureNames: FeatureName[] = [
  'Dragging Reparents By Default',
  'Dragging Shows Overlay',
]

let FeatureSwitches: { [feature: string]: boolean } = {
  'Dragging Reparents By Default': false,
  'Dragging Shows Overlay': false,
}

export function isFeatureEnabled(featureName: FeatureName): boolean {
  return FeatureSwitches[featureName] ?? false
}

export function toggleFeatureEnabled(featureName: FeatureName) {
  FeatureSwitches[featureName] = !isFeatureEnabled(featureName)
}
