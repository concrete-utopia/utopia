var fs = require('fs');

module.exports = function linuxProvider() {
  var sysinfo = require('../../sysinfo.js')();
  var historyCpuUsage = {};

  return {
    lookup: lookup,
    clearHistory: clearHistory
  };

  function lookup(pid, options, callback) {
    if(typeof options == 'function') {
        callback = options;
        options = {};
    }
    options = options || {};

    var uptime;
    getUptime(function(err, value) {
      if(err) {
        callback(err);
      } else {
        uptime = value;
        getProcStat(pid, calculateUsage);
      }
    });

    function calculateUsage(err, stat) {
      if(err) {
        callback(err);
      } else {
        var usageData = {};

        if(historyCpuUsage[pid] && options.keepHistory) {
          var cpuUsage = calculateCpuUsageFromHistory(sysinfo, uptime, stat, historyCpuUsage[pid]);
        } else {
          var cpuUsage = calculateCpuUsage(sysinfo, uptime, stat);
        }

        // memeory info
        usageData.memory = calculateMemoryUsage(sysinfo, stat);
        usageData.memoryInfo = {
          rss: calculateMemoryUsage(sysinfo, stat),
          vsize: calculateVirtualMemoryUsage(sysinfo, stat)
        };

        // cpu info
        usageData.cpu = cpuUsage.pcpu;
        usageData.cpuInfo = {
          pcpu: cpuUsage.pcpu,
          pcpuUser: cpuUsage.pcpuUser,
          pcpuSystem: cpuUsage.pcpuSystem,
          cpuTime: cpuUsage.cpuTime
        };

        if(options.keepHistory) {
          //save totalTime in history
          historyCpuUsage[pid] = {
            timestamp: Date.now(),
            stat: stat,
            uptime: uptime
          };
        }

        callback(null, usageData);
      }
    }
  };

  function clearHistory (pid) {
    if(pid) {
      if(historyCpuUsage[pid]) {
        historyCpuUsage[pid] = null;
      }
    } else {
      historyCpuUsage = {};
    }
  }
};

function calculateCpuUsage(sysinfo, uptime, stat) {
  var userTime = stat.utime / sysinfo.HERTZ;
  var systemTime = stat.stime / sysinfo.HERTZ;
  var totalTime = (stat.stime + stat.utime) / sysinfo.HERTZ;
  var processUptime = uptime - stat.startTime / sysinfo.HERTZ;
  return {
    pcpu: (totalTime / processUptime) * 100,
    pcpuUser: (userTime / processUptime) * 100,
    pcpuSystem: (systemTime / processUptime) * 100
  };
}

function calculateCpuUsageFromHistory(sysinfo, uptime, stat, lastUsage) {
  var userTime = stat.utime / sysinfo.HERTZ;
  var systemTime = stat.stime / sysinfo.HERTZ;
  var totalTime = userTime + systemTime;

  var lastUserTime = lastUsage.stat.utime / sysinfo.HERTZ;
  var lastSystemTime = lastUsage.stat.stime / sysinfo.HERTZ;
  var lastTotalTime = lastUserTime + lastSystemTime;

  var diffTotal = totalTime - lastTotalTime;
  var diffUser = userTime - lastUserTime;
  var diffSystem = systemTime - lastSystemTime;
  var timeSpent = uptime - lastUsage.uptime;

  return {
    pcpu: (diffTotal / timeSpent) * 100,
    pcpuUser: (diffUser / timeSpent) * 100,
    pcpuSystem: (diffSystem / timeSpent) * 100,
    cpuTime: diffTotal
  };
}

function calculateMemoryUsage(sysinfo, stat) {
  return stat.rss * sysinfo.PAGE_SIZE;
}

function calculateVirtualMemoryUsage(sysinfo, stat) {
  return stat.vsize * sysinfo.PAGE_SIZE;
}

function getUptime(callback) {
  fs.readFile('/proc/uptime', 'utf8', function(err, data) {
    if(err) {
      callback(err);
    } else {
      var matched = data.match(/^(.*) /);
      if(matched) {
        var uptime = parseFloat(matched[1]);
        callback(null, uptime);
      } else {
        callback(new Error('Invalid formatted uptime file'));
      }
    }
  });
}

var STAT_INDEXES = {
  STIME: 11,
  UTIME: 12,
  START_TIME: 19,
  RSS: 21,
  VSIZE: 20
};

function getProcStat(pid, callback) {
  var fileName = '/proc/' + pid + '/stat';
  fs.readFile(fileName, 'utf8', function(err, data) {
    if(err) {
      callback(err);
    } else {
      data = data.substr(data.lastIndexOf(')') + 2);
      var parts = data.split(' ');
      var statObject = {
        stime: parseFloat(parts[STAT_INDEXES.STIME]),
        utime: parseFloat(parts[STAT_INDEXES.UTIME]),
        startTime: parseFloat(parts[STAT_INDEXES.START_TIME]),
        rss: parseFloat(parts[STAT_INDEXES.RSS]),
        vsize: parseFloat(parts[STAT_INDEXES.VSIZE])
      };
      callback(null, statObject);
    }
  });
}
