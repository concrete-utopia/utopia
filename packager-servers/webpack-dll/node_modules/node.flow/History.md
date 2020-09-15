# History

## 1.2.3 / 2013-12-12

- [update packages] node.extend->1.0.8, should->2.1.1



## 1.2.2 / 2012-12-31

- [refactoring] Passing variable to error handler



## 1.2.1 / 2012-12-30

- [refactoring] Catch error when first argument is an instance of Error
- [update packages] should->1.2.1



## 1.2.0 / 2012-11-21

- [new feature] Add error handler



## 1.1.3 / 2012-07-23

- Update examples



## 1.1.2 / 2012-07-19

- [refactoring] `.end()` deos not need to take a callback
- [update packages] mongoose->2.7.1, should->1.0.0



## 1.1.1 / 2012-03-31

- [refactoring] Error handling when no parallel task assigned before calling `join`



## 1.1.0 / 2012-02-14

- [new feature] Pass is_parallel to have better look args from last stack
- [refactoring] Use Object.keys forEAch instead of for in
- [update packages] mongoose->2.5.7 for test
- Added more tests



## 1.0.0 / 2012-02-14

- [bug fix] Parallel arguments from last stack should be overwritten by default arguments
- Added full test



## 0.1.0 / 2012-02-13

- [bug fix] Cant pass arguments to parallel fn from series fn
- Read version number from package.json
- Added basic tests



## 0.0.3 / 2012-01-16

- [bug fix] Clear arguments passed from ready method before running the next task



## 0.0.2 / 2012-01-16

- [bug fix] Merge arguments from parallel tasks and pass to the next task



## 0.0.1 / 2012-01-10

- Initial release
