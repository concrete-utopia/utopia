import slugify from 'slugify'
import { SLUGIFY_OPTIONS } from '../routes/internal.projects.$id.rename'

describe('slugify', () => {
  const tests: { name: string; input: string; wanted: string }[] = [
    {
      name: 'a single word',
      input: 'Hello',
      wanted: 'hello',
    },
    {
      name: 'alphanumeric',
      input: 'the Answer is 42',
      wanted: 'the-answer-is-42',
    },
    {
      name: 'multiple words, mixed case',
      input: 'Hello WoRld',
      wanted: 'hello-world',
    },
    {
      name: 'multiple words with trailing non alphanumeric',
      input: '¡hello WORLD!',
      wanted: 'hello-world',
    },
    {
      name: 'multiple spaces',
      input: 'some    large  spacing!!',
      wanted: 'some-large-spacing',
    },
    {
      name: 'empty string',
      input: '',
      wanted: '',
    },
    {
      name: 'uppercase',
      input: 'ALLCAPS',
      wanted: 'allcaps',
    },
    {
      name: 'only non alphanumeric',
      input: '!!?!',
      wanted: '',
    },
  ]
  for (let i = 0; i < tests.length; i++) {
    const test = tests[i]
    it(`${i + 1}/${tests.length} ${test.name}`, async () => {
      expect(slugify(test.input, SLUGIFY_OPTIONS)).toEqual(test.wanted)
    })
  }
})
