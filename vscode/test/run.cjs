const {resolve, join} = require('path')
const os = require('os')
const {mkdtempSync, rmSync} = require('fs')
const {downloadAndUnzipVSCode, runTests} = require('@vscode/test-electron')

async function main() {
  let tempRoot
  try {
    const cachePath = resolve(__dirname, '../../.obj/vscode-test')
    const extensionDevelopmentPath = resolve(__dirname, '..')
    const extensionTestsPath = resolve(__dirname, 'tests.cjs')
    const workspace = resolve(__dirname, 'fixture')
    // Use a fresh user profile to avoid loading user/global VS Code settings,
    // in order to make the tests deterministic.
    tempRoot = mkdtempSync(join(os.tmpdir(), 'rewrap-vscode-test-user-data-'))
    const userDataDir = tempRoot
    const launchArgs = [
      workspace,
      '--disable-extensions',
      `--user-data-dir=${userDataDir}`,
    ]

    // Download manually so we can choose the location
    vscodeExecutablePath = await downloadAndUnzipVSCode({cachePath})

    // Run the integration tests
    await runTests({extensionDevelopmentPath, extensionTestsPath, launchArgs, reuseMachineInstall: true, vscodeExecutablePath})
  }
  catch (err) {
    console.error("Failed to run tests")
    console.error(err)
    process.exit(1)
  }
  finally {
    if (typeof tempRoot === 'string') {
      try {
        rmSync(tempRoot, {recursive: true, force: true})
      } catch {
        // Best-effort cleanup only.
      }
    }
  }
}

main()
