import { RecoilRoot, atom, selector, useRecoilState, useRecoilValue } from 'recoil'

export const layoutHoveredState = atom<boolean>({
  key: 'layoutHoveredState', // unique ID (with respect to other atoms/selectors)
  default: false, // default value (aka initial value)
})
