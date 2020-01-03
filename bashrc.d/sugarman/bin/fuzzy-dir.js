#!/usr/bin/env node

// # Usage
// ./fuzzy-dir.js pattern [--dir ~/my/path ...] [--full-path]

const fs = require('fs');
const path = require('path');
const childProcess = require('child_process');

function parseArgs(argv) {
  const argEndIndex = argv.includes('--') ? argv.indexOf('--') : argv.length;
  const args = {};
  let key = '_';
  for (let i = 2; i < argEndIndex; i++) {
    if (argv[i].startsWith('--')) {
      // "my-arg" -> "myArg"
      key = argv[i].slice(2).replace(/-([^-])/g, (_, m) => m.toUpperCase())
      continue;
    }
    const value = argv[i];
    args[key] = (args[key] || []).concat(value);
  }

  if (args['_'] === undefined) {
    console.error('please specify a pattern');
    process.exit(1);
  }

  return {
    dirs: args.dir || [process.cwd()],
    pattern: args['_'][0],
    useFullPath: args.fullPath,
  }
}

function failWithPathError(dir) {
  console.error('airbnb-cd failed to find path', dir);
  process.exit(1);
}

function getFileInfoList(dir) {
  let files;
  try {
    files = fs.readdirSync(dir);
  } catch (e) {
    failWithPathError(dir);
  }
  if (files.length === 0) {
    failWithPathError(dir);
  }
  return files.map(file => ({ name: file, fullPath: path.join(dir, file) }));
}

// TODO: use --full-path option, when true returns full path, when false returns just the dir name.
// This is useful for autocompleting things like `ktail hhl` -> `k tail SERVICE="homes-host-loop`
// Could even add --filter="*-loop"

// TODO: improve fuzzy matching so:
//  - `loop` matches `loopz` over `zloop` and `loozp`
//  - `hhl` matches `homes-host-loop` over `highly`
//  - `hlo` matches `homes-host-loop` over `highly-optimistic`

// For each word in directory name (split by case change or non-alphanumeric character),
// give points for:
//  1. matching the first character
//  2. matching multiple characters in a row
//  3. matching a high percentage of the characters
function getFuzzyMatcher(pattern) {
  // Use case sensitivity if the pattern contains an upper case character.
  const caseSensitive = /[A-Z]/.test(pattern);
  const patternChars = [...pattern];

  return dir => {
    // "UserIDCard" -> ["User", "ID", "Card"]
    const words = dir
      .replace(/([a-z])(?=[A-Z])/g, '$1_')
      .replace(/([A-Z]+)(?=[A-Z][a-z])/g, '$1_')
      .split(/[^a-zA-Z]+/g);

    if (!caseSensitive) {
      dir = dir.toLowerCase();
      words = words.map(word => word.toLowerCase());
    }

    let index = 0;
    return patternChars.every(c => {
      for (; index < dir.length; index++) {
        if (c === dir[index]) return true;
      }
    });

    const patternChars = [...pattern];
  };
}

function getFuzzyMatcher(pattern) {
  const caseSensitive = /[A-Z]/.test(pattern);
  const patternChars = [...pattern];
  return fileInfo => {
    let { name } = fileInfo;
    if (!caseSensitive) {
      name = name.toLowerCase();
    }
    let index = 0;
    const isMatch = patternChars.every(c => {
      for (; index < name.length; index++) {
        if (c === name[index]) return true;
      }
    });
    const score = isMatch && pattern.length / name.length;
    return { score, fileInfo };
  };
}

function getComparator(key) {
  return (a, b) => b[key] - a[key];
}

const { pattern, dirs, useFullPath } = parseArgs(process.argv);
const fuzzyMatch = getFuzzyMatcher(pattern);
const fileInfoList = dirs.reduce((result, dir) => result.concat(getFileInfoList(dir)), []);
const { score, fileInfo } = fileInfoList.map(fuzzyMatch).sort(getComparator('score'))[0];
if (!score) {
  console.error('no directories match', pattern);
  exit(1);
} else {
  console.log(useFullPath ? fileInfo.fullPath : fileInfo.name);
}
