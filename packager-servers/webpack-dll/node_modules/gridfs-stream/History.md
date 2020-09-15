0.5.3 / 2014-10-23
==================

 * fixed; 'exist' method does not respect 'root' option #50 [nachoalthabe](https://github.com/nachoalthabe)

0.5.2 / 2014-10-23
==================

 * fixed; multiple, multi-chunk stream support #47 [ceari](https://github.com/ceari)

0.5.1 / 2014-05-14
==================

 * fixed; support 0-byte files #39 [PNPhillips](https://github.com/PNPhillips)
 * fixed; do not overwrite the filename with empty string #38 [vsivsi](https://github.com/vsivsi)

0.5.0 / 2014-05-03
==================

 * added; range read support #37 from [xissy](https://github.com/xissy)
 * added; support for file existance queries #32 [jaalfaro](https://github.com/jaalfaro)
 * docs; show that Mongo Db needs to be initalized #33 [marcusoftnet](https://github.com/marcusoftnet)

0.4.1 / 2014-01-22
==================

 * fixed; Changing chunk_size to chunkSize #27 from [khous](https://github.com/khous)
 * fixed; Server is undefined, should be mongo.Server instead #23 from [sahat](https://github.com/sahat)

0.4.0 / 2013-04-07
==================

  * changed; ReadStream/WriteStream api #4 #6 #7 #10 #11 #15 [Reggino](https://github.com/Reggino)
  * fixed; parameter ambiguity (objectId / filename) for GridReadStream, GridWriteStream, grid#remove() #11 [Reggino](https://github.com/Reggino)
  * fixed; Gridfs should be able to store 12 lettered file names Issue #11 [Reggino](https://github.com/Reggino)
  * fixed; ReadStream pause() / resume() issue #12 #13 [Reggino](https://github.com/Reggino)
  * fixed; #4
  * fixed; #6
  * fixed; #7
  * fixed; #10

0.3.2 / 2012-10-25
==================

  * add; passing File object when emmiting close event [diogogmt](https://github.com/diogogmt)
  * update readme

0.3.1 / 2012-09-12
==================

  * add; remove method

0.3.0 / 2012-09-12
==================

  * refactor api
  * added; grid#files & grid#collection()

0.2.1 / 2012-09-12
==================

  * explicit id creation

0.2.0 / 2012-09-11
==================

  * update driver to 1.1.7
  * auto cast to ObjectId when possible

0.1.1 / 2012-08-31
==================

  * updated; mongodb version to 1.1.5

0.1.0 / 06-13-2012
==================

  * fixed; node-mongodb-native compatibility issues (#1)

0.0.5 / 05-18-2012
==================

  * fixed; node v4 compatibility

0.0.4 / 05-11-2012
==================

  * fixed; do not flush until open
  * updated; mongodb driver

0.0.3 / 05-10-2012
==================

  * fixed; only open once

0.0.2 / 05-10-2012
==================

  * add GridWriteStream progress event

0.0.1 / 05-09-2012
==================

  * initial release

