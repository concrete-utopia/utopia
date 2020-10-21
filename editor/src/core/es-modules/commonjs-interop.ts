// This functionality is based on the import helpers created by TS when the `esModuleInterop` flag is set to `true`,
// all of which are created in https://github.com/microsoft/TypeScript/blob/master/src/compiler/factory/emitHelpers.ts#L810

function isESModule(mod: unknown): boolean {
  return mod != null && typeof mod === 'object' && (mod as any).__esModule
}

export function importStar(mod: unknown): unknown {
  if (isESModule(mod)) {
    return mod
  } else {
    let result: any = { default: mod }

    if (typeof mod === 'object' && mod != null) {
      for (var k in mod) {
        if (k !== 'default' && mod.hasOwnProperty(k)) {
          result[k] = (mod as any)[k]
        }
      }
    }

    return result
  }
}

export function importDefault(mod: unknown): unknown {
  return isESModule(mod) ? (mod as any).default : mod
}
