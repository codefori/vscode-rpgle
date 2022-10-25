const specificTests = process.argv[2];

async function run() {
  const suite = require(`./suite`);
  let testNames = Object.keys(suite);

  if (specificTests) {
    testNames = testNames.filter(name => name.startsWith(specificTests));
  }

  console.log(`Running ${testNames.length} tests:`);

  const start = process.hrtime();

  for (const testName of testNames) {
    const test = suite[testName];
    await test();
  }

  const elapsed = process.hrtime(start)[1] / 1000000; // divide by a million to get nano to milli
  console.log(process.hrtime(start)[0] + `s, ` + elapsed.toFixed(3) + ` ms`); // print message + time

};

run();