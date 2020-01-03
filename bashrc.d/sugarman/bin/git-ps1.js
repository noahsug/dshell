#!/usr/bin/env node

const childProcess = require('child_process');

function exec(cmdString, { useStderrAsStdout } = {}) {
  const maxBuffer = 20 * 1024 * 1024; // set to 20 MiB
  return new Promise((resolve, reject) => {
    childProcess.exec(cmdString, { maxBuffer }, (err, stdout, stderr) => {
      if (err) {
        reject(err);
        return;
      }
      if (useStderrAsStdout) {
        resolve(stderr.trim());
        return;
      }
      if (stderr) {
        console.error(stderr);
      }
      resolve(stdout.trim());
    });
  });
}
