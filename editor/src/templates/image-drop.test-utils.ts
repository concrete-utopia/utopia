const NO_OP = () => {
  /* noop */
}
export const IMAGE_DROP_HOOK: { current: () => void } = { current: NO_OP }
export const FOR_TESTS_SET_DROP_HOOK = (hook: () => void): void => {
  IMAGE_DROP_HOOK.current = hook
}

export const FOR_TESTS_UNSET_DROP_HOOK = (): void => {
  IMAGE_DROP_HOOK.current = NO_OP
}
