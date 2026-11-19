// frontends/clysm/tests/web-shell.js — Electron as a plain browser: a window without
// Node.js shows the page LEM_WEB_PAGE (a file), driven by the module
// CLYSM_ELECTRON_TEST as in the desktop application's tests.
const { app, BrowserWindow } = require('electron');
const path = require('node:path');

// CLYSM_USER_DATA: the directory of the browser's data (localStorage...)
if (process.env.CLYSM_USER_DATA) app.setPath('userData', process.env.CLYSM_USER_DATA);

app.whenReady().then(() => {
  const win = new BrowserWindow({ show: false, width: 960, height: 640, webPreferences: { offscreen: true } });
  require(path.resolve(process.env.CLYSM_ELECTRON_TEST))(win, app);
  win.loadFile(process.env.LEM_WEB_PAGE);
});
