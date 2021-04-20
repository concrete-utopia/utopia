export interface UtopiaVSCodeConfig {
  followSelection: {
    enabled: boolean
  }
}

export function defaultConfig(): UtopiaVSCodeConfig {
  return {
    followSelection: {
      enabled: true,
    },
  }
}
