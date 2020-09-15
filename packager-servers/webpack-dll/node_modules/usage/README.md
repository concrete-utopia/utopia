# node-usage [![Build Status](https://travis-ci.org/arunoda/node-usage.png?branch=master)](https://travis-ci.org/arunoda/node-usage)

### process usage lookup with nodejs

* Simple interface to lookup cpu and memory usage of any accessible process on the system.
* Works on OSX, Linux, SmartOS and Solaris
* Tested on Heroku, Nodejitsu and Modulus

## Example

### Code
~~~js
var usage = require('usage');

var pid = process.pid // you can use any valid PID instead
usage.lookup(pid, function(err, result) {

});
~~~

### Result Object
~~~js
{
	memory: 100065280, // in no of bytes
	memoryInfo: {
		rss: 15966208, // resident size memory in bytes
		vsize: 3127906304 // virtual memory size in bytes
	},
	cpu: 10.6 // in percentage
}
~~~

## Average CPU usage vs Current CPU usage
>This is only applicable for Linux

By default CPU Percentage provided is an average from the starting time of the process. It does not correctly reflect the current CPU usage. (this is also a problem with linux `ps` utility)

But If you call `usage.lookup()` continuously for a given pid, you can turn on **keepHistory** flag and you'll get the CPU usage since last time you track the usage. This reflects the current CPU usage.

see following example to enable keepHistory flag

~~~js
var pid = process.pid;
var options = { keepHistory: true }
usage.lookup(pid, options, function(err, result) {

});
~~~

you can clear history cache too
~~~js
usage.clearHistory(pid); //clear history for the given pid
usage.clearHistory(); //clean history for all pids
~~~
