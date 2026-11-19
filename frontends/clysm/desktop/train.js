// train.js — build.sh: started as Electron's test module (CLYSM_ELECTRON_TEST)
// with CLYSM_IMAGE_TRAIN set, the page starts Lem from the heap image with
// the functions noting that they run (host/web/clysm.mjs); once the editor is
// shown, the functions that ran are written to CLYSM_IMAGE_TRAIN, for
// hint-image.mjs.
const fs = require('node:fs');

module.exports = (win, app) => {
  const wc = win.webContents;
  wc.once('did-finish-load', async () => {
    for (let i = 0; i < 1500; i++) {
      const shown = await wc.executeJavaScript("document.querySelectorAll('.lem-line').length > 0");
      if (shown) break;
      await new Promise((resolve) => setTimeout(resolve, 20));
    }
    // (what runs just after: the first frames, the timers of the editor)
    await new Promise((resolve) => setTimeout(resolve, 500));
    const ran = await wc.executeJavaScript('JSON.stringify(globalThis.__clysm_training ? globalThis.__clysm_training() : null)');
    fs.writeFileSync(process.env.CLYSM_IMAGE_TRAIN, ran);
    app.exit(0);
  });
};
