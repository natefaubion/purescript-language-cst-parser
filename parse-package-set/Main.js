import { mkdtempSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { hrtime } from "process";

const tmpdirImpl = function (prefix) {
  return () => mkdtempSync(join(tmpdir(), prefix), "utf-8");
};
export { tmpdirImpl as tmpdir };

const hrtimeImpl = function () {
  const t = hrtime();
  return { seconds: t[0], nanos: t[1] };
};
export { hrtimeImpl as hrtime };

export function hrtimeDiff(old) {
  return () => {
    const t = hrtime([old.seconds, old.nanos]);
    return { seconds: t[0], nanos: t[1] };
  };
}
