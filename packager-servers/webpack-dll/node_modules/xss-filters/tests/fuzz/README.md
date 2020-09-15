# fuzzer

A quick little fuzzer for detecting issues within each of the five context areas.

# How it works

The fuzzer creates testcases by mutating entries within [bin/seed.xss](bin/seed.xss). The entries are then stored into redis as:

```
redis:
	testcases:1 -> "></div><img onerror=alert(1) src=x>"
	testcases:2 -> "alert(1)"
	testcases:3 -> "`'`;';`alert(1)"

	testcases_counter: 4
```

The fuzzer then spawns a headless browser (zombie.js), which requests the newly created test case. An example page would be localhost:1339/testcase/3

The web server loads the payload from redis, and serves the following html page:

```html
<html> <body> <h1> Fuzz Test! </h1>

  <script>window.alert = function(msg) { console.log(\"" + id + ":\"+ msg) } </script>
  <script>window.prompt = function(msg) { console.log(\"" + id + ":\"+ msg) } </script>

  <!-- <script>alert('xssssss')</script> -->
  <!-- <SCRIPT SRC='http://c0nrad.io'></SCRIPT> -->

  <div style='border: 1px solid'>
    <h2> xssFilters.inHTMLData(fuzz) </h1>
    <div> xssFilters.inHTMLData(fuzz) </div>
  </div>


  <div style='border: 1px solid'>
  <h2> xssFilters.inHTMLComment(fuzz) </h2>
  <!-- + xssFilters.inHTMLComment(fuzz) + -->
  </div>" +


  <div style='border: 1px solid'>
  <h2> xssFilters.inSingleQuotedAttr(fuzz) </h2>
  <input value='" + xssFilters.inSingleQuotedAttr(fuzz) + "'/>
  </div>


  <div style='border: 1px solid'>
  <h2> xssFilters.inDoubleQuotedAttr(fuzz) </h2>
  <input value=\"" + xssFilters.inDoubleQuotedAttr(fuzz) + "\"/>
  </div>


  <div style='border: 1px solid'>
  <h2> xssFilters.inUnQuotedAttr(fuzz) </h2>
  <input value=" + xssFilters.inUnQuotedAttr(fuzz) + "'/>
</body> </html>
```

If the headless browser detects anything (console.logs, xhr, redirects), it stores that testcase in redis:injections

# To Run

First you must install Redis. There's plenty of resources on how to do this.

You also need a more recent version of Node to use zombie.js. I'm using version v0.12.2.

First you start the server:

```
$ pwd
~/xss-filters/tests/fuzz
$ node app.js
```

Then run a bunch of fuzzers:
```
$ node --harmony bin/fuzzer.js
```

To check the results:
```
$ redis
> get testcases_counter
"3391770"
> smembers injections
(empty list or set)
```

Which means I have no results yet.

Enjoy!

## Contact

stuartlarsen@yahoo-inc.com
