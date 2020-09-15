# History

## 1.1.0 / 2015-02-16

- [new feature] Added the ability to pass in an optional alternative file system to remove the files



## 1.0.4 / 2014-01-09

- [bug fix] Return `callback` instead of `ready` after `unlink` files



## 1.0.3 / 2013-12-18

- [bug fix] Throw exception if fir or file doesn't exist



## 1.0.2 / 2013-12-12

- [update packages] node.flow->1.2.3



## 1.0.1 / 2013-12-11

- [bug fix] Use async api to avoid error
- [refactoring] Use `fs.lstat` instead of `fs.stat` for `symlink`



## 1.0.0 / 2012-07-23

- [refactoring] Use `fs` instead of `path` for node v0.8.x



## 0.0.3 / 2012-07-08

- [bug fix] Excute callback with remove single file



## 0.0.2 / 2012-07-08

- Remove single file as well



## 0.0.1 / 2012-07-08

- Initial release
- Renamed from rmdirr
- [bug fix] Extra slash at the end
