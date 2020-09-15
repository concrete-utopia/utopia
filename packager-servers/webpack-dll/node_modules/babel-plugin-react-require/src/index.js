export default function ({ types: t }) {
  return {
    visitor: {
      JSXOpeningElement(path, { file }) {
        file.set('hasJSX', true);
      },

      Program: {
        enter(path, { file }) {
          file.set('hasJSX', false);
        },

        exit({ node, scope }, { file }) {
          if (!(file.get('hasJSX') && !scope.hasBinding('React'))) {
            return;
          }

          const reactImportDeclaration = t.importDeclaration([
            t.importDefaultSpecifier(t.identifier('React')),
          ], t.stringLiteral('react'));

          node.body.unshift(reactImportDeclaration);
        },
      },
    },
  };
}
