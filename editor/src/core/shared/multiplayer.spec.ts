import { multiplayerInitialsFromName } from './multiplayer'

describe('multiplayer', () => {
  describe('multiplayerInitialsFromName', () => {
    const tests: {
      name: string
      input: string
      want: string
    }[] = [
      {
        name: 'regular name',
        input: 'Foo Bar',
        want: 'FB',
      },
      {
        name: 'name with many words',
        input: 'Foo Bar Baz',
        want: 'FB',
      },
      {
        name: 'single word',
        input: 'Bar',
        want: 'BA',
      },
      {
        name: 'a very short name',
        input: 'F',
        want: 'FX',
      },
      {
        name: 'no name at all',
        input: '',
        want: 'XX',
      },
    ]
    tests.forEach((test, index) => {
      it(`${index}/${tests.length + 1} ${test.name}`, async () => {
        const got = multiplayerInitialsFromName(test.input)
        expect(got).toEqual(test.want)
      })
    })
  })
})
