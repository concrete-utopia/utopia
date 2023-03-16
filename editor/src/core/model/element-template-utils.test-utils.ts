import { MOCK_NEXT_GENERATED_UIDS, MOCK_NEXT_GENERATED_UIDS_IDX } from '../shared/uid-utils'

export function FOR_TESTS_setNextGeneratedUid(nextUid: string): void {
  MOCK_NEXT_GENERATED_UIDS.current = [nextUid]
  MOCK_NEXT_GENERATED_UIDS_IDX.current = 0
}

export function FOR_TESTS_setNextGeneratedUids(uids: Array<string>): void {
  MOCK_NEXT_GENERATED_UIDS.current = uids
  MOCK_NEXT_GENERATED_UIDS_IDX.current = 0
}

export function FOR_TESTS_CLEAR_MOCK_NEXT_GENERATED_UIDS(): void {
  MOCK_NEXT_GENERATED_UIDS.current = []
  MOCK_NEXT_GENERATED_UIDS_IDX.current = 0
}

// automatic cleanup after tests
afterEach(() => {
  FOR_TESTS_CLEAR_MOCK_NEXT_GENERATED_UIDS()
})
