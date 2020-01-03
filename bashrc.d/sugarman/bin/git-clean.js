#!/usr/bin/env node

const AUTHOR = 'Noah Sugarman';

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

function parseArgvFlags() {
  const { argv } = process;
  const argEndIndex = argv.includes('--') ? argv.indexOf('--') : argv.length;
  const args = {};
  let key = '_';
  for (let i = 2; i < argEndIndex; i++) {
    if (argv[i].startsWith('-')) {
      // "my-arg" -> "myArg"
      key = argv[i].replace(/^[-]+/, '').replace(/-([^-])/g, (_, m) => m.toUpperCase());
      args[key] = args[key] || [];
      continue;
    }
    const value = argv[i];
    args[key].push(value);
  }
  return args;
}

function parseOptions(args) {
  return {
    dryrun: args.dryrun || args.D,
  };
}

async function getExistingBranches() {
  const gitOutput = await exec('git branch --format="%(refname:short)" --sort=-committerdate');
  const branches = gitOutput.split('\n');
  return branches.filter(branch => branch !== 'master');
}

async function getDeletedBranches() {
  const gitOutput = await exec('git fetch origin master --prune', { useStderrAsStdout: true });
  const regex = /\[deleted\].*origin\/(.*)/g;
  const result = [];
  while ((match = regex.exec(gitOutput))) {
    result.push(match[1]);
  }
  return result;
}

function getCurrentBranch() {
  return exec('git symbolic-ref --short HEAD');
}

// Returns branches whose last commit message exactly matches a merged commit message.
async function getSquashMergedBranches(branches) {
  const gitLogMasterOutput = await exec(`git log origin/master --author="${AUTHOR}" --format="%s" | head -n25`);
  const mergedCommitMessagesWithCommitNumber = gitLogMasterOutput.split('\n');
  const mergedCommitMessages = new Set(
    gitLogMasterOutput
      .split('\n')
      // "Commit message (#1161)" -> "Commit message"
      .map(msg => msg.replace(/ \(.*\)$/, ''))
  );
  const isSquashMergedByIndex = await Promise.all(
    branches.map(async branch => {
      const commit = await exec(`git log ${branch} -- --format="%s" | head -n1`);
      return mergedCommitMessages.has(commit);
    })
  );
  return branches.filter((branch, index) => isSquashMergedByIndex[index]);
}

async function getMergedBranches() {
  const gitOutput = await exec('git branch --format="%(refname:short)" --merged');
  const branches = gitOutput.split('\n');
  return branches.filter(branch => branch !== 'master');
}

function switchToSafeBranch(branches, branchesToDelete) {
  const safeBranch = branches.find(branch => !branchesToDelete.includes(branch));
  if (safeBranch) {
    console.log(`switching to branch "${safeBranch}"`);
    return exec(`git checkout ${safeBranch} --`);
  }

  // We're deleting ALL of our branches, switch to origin/master.
  console.log('checking out "origin/master"');
  return exec('git checkout origin/master');
}

async function run() {
  const { dryrun } = parseOptions(parseArgvFlags());
  if (dryrun) {
    console.log('running in dry-run mode');
  }

  const [
    branches,
    deletedBranches,
    currentBranch,
  ] = await Promise.all([
    getExistingBranches(),
    getDeletedBranches(),
    getCurrentBranch(),
  ]);
  // wait for "git fetch origin/master"
  const [
    mergedBranches,
    squashMergedBranches,
  ] = await Promise.all([
    getMergedBranches(),
    getSquashMergedBranches(branches),
  ]);

  const deletedBranchesSet = new Set(deletedBranches);
  const mergedBranchesSet = new Set(mergedBranches);
  const squashMergedBranchesSet = new Set(squashMergedBranches);
  const toDelete = branches.filter(
    branch =>
      deletedBranchesSet.has(branch) ||
      mergedBranchesSet.has(branch) ||
      squashMergedBranchesSet.has(branch)
  );

  // switch branches if we're on a branch that's about to be deleted
  if (toDelete.includes(currentBranch)) {
    await switchToSafeBranch(branches, toDelete);
  }

  if (toDelete.length > 0) {
    if (dryrun) {
      console.log(toDelete.map(b => `Deleted branch ${b}`).join('\n'));
    } else {
      const output = await exec(`git branch -D ${toDelete.join(' ')}`);
      console.log(output);
    }
  } else {
    console.error('found no branches to delete');
  }
}

run().catch(e => console.error(e));
