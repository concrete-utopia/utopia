const babel = require('babel-core');
const assert = require('assert');

let reactPlugin;
let transform;

try {
  reactPlugin = require('../lib-cov/index').default; // eslint-disable-line import/no-unresolved
} catch (error) {
  reactPlugin = require('../src/index').default;
}

describe('babel-plugin-react', () => {
  beforeEach(() => {
    transform = code => babel.transform(code, {
      plugins: ['syntax-jsx', reactPlugin],
    }).code;
  });

  it('should return transpiled code with required React', () => {
    const transformed = transform('export default class Component {render() {return <div />}}');

    assert.equal(transformed, 'import React from "react";\nexport default class Component {\n  render() {\n    return <div />;\n  }\n}');
  });

  it('should return not transpiled code', () => {
    const transformed = transform('console.log("hello world")');

    assert.equal(transformed, 'console.log("hello world");');
  });

  it('should check that plugin does not import React twice', () => {
    const transformed = transform('class Component{render(){return <div/>}} class Component2{render(){return <div />}}');

    assert.equal(transformed, 'import React from "react";\nclass Component {\n  render() {\n    return <div />;\n  }\n}'
      + 'class Component2 {\n  render() {\n    return <div />;\n  }\n}');
  });

  it('should does not replace users import on plugins import', () => {
    const transformed = transform('import React from"react/addons"\nclass Component{render(){return <div/>}}');

    assert.equal(transformed, 'import React from "react/addons";\nclass Component {\n  render() {\n    return <div />;\n  }\n}');
  });
});
