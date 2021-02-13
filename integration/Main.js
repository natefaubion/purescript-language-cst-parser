const fs = require("fs");
const os = require("os");
const path = require("path");
const process = require("process");

exports.tmpdir = (prefix) => () =>
  fs.mkdtempSync(path.join(os.tmpdir(), prefix), "utf-8");

exports.hrtime = () => {
  const t = process.hrtime()
  return { seconds: t[0], nanos: t[1] };
};

exports.hrtimeDiff = (old) => () => {
  const t = process.hrtime([old.seconds, old.nanos]);
  return { seconds: t[0], nanos: t[1] };
};
