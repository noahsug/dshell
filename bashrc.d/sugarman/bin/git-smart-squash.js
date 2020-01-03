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

async function run() {
  const commits = getUnmergedCommits();
  const changedFiles = getWorkingChanges();
  const commitToChangesMap = mapCommitsToChanged(commits, changedFiles);

  if (isDryrun()) {
    console.log(commitToChangesMap);
    return;
  }

  const workingChangesSha = commitWorkingChanges();

  resetToMergeBase();

  commits.reverse().forEach(commit => {
    cherryPick(commit);
    const toSquash = commitToChangesMap(commit);
    if (toSquash.length > 0) {
      checkout(workingChangesSha, toSquash);
      squash();
    }
  });

  const workingChanges = commitToChangesMap('HEAD');
  addWorkingChanges(workingChanges);
}

run().catch(e => console.error(e));
