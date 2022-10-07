export let ALWAYS_SHOW_PADDING_CONTROLS: boolean = false

export function SET_ALWAYS_SHOW_PADDING_CONTROLS(): void {
  ALWAYS_SHOW_PADDING_CONTROLS = true
}

afterEach(() => {
  ALWAYS_SHOW_PADDING_CONTROLS = false
})
