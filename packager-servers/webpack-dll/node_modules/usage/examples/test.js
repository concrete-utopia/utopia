var usage = require('../');
var pid = (process.argv[2])? parseInt(process.argv[2]): process.pid;

setInterval(function() {

	var options =  { keepHistory: true };
	usage.lookup(pid, options, function(err, stat) {

		console.log(err, stat);
	});
}, 2000);