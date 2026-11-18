// 합성 앱을 실제 Chrome에서 검증하고 일회성 인증과 프로세스를 종료한다.
const fs = require('node:fs');
const path = require('node:path');
const cp = require('node:child_process');
const crypto = require('node:crypto');
const net = require('node:net');
const { chromium } = require('../.runtime/browser/node_modules/playwright-core');
const root = path.resolve(__dirname, '..');
const output = path.join(__dirname, 'output');
fs.mkdirSync(output, { recursive: true });

// 역할: 사용 중인 서비스를 건드리지 않고 빈 로컬 포트를 확보한다.
async function freePort() {
  const server = net.createServer();
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const port = server.address().port;
  await new Promise(resolve => server.close(resolve));
  return port;
}

// 역할: Shiny 화면 처리가 끝날 때까지 짧은 제한 안에서 기다린다.
async function settled(page) {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 10000 });
}

// 역할: 일회성 서버와 브라우저로 인증·조회·다운로드를 검사한다.
async function main() {
  const port = await freePort();
  const password = crypto.randomBytes(24).toString('hex');
  let log = '';
  const server = cp.spawn(path.join(root, '.runtime/R/app/bin/x64/Rscript.exe'), ['--vanilla', 'tests/browser-app.R'], {
    cwd: root, windowsHide: true,
    env: { ...process.env, R_HOME: path.join(root, '.runtime/R/app'), R_LIBS_USER: path.join(root, '.runtime/library'), LC_ALL: 'English_United States.utf8', CLOTHING_TEST_PASSWORD: password, CLOTHING_TEST_PORT: String(port) }
  });
  server.stdout.on('data', chunk => { log += chunk.toString(); });
  server.stderr.on('data', chunk => { log += chunk.toString(); });
  let browser;
  const report = { synthetic: true, checks: [], errors: [] };
  try {
    for (let attempt = 0; attempt < 100 && !log.includes('Listening on'); attempt++) {
      if (server.exitCode !== null) throw new Error('합성 앱 시작 실패');
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    browser = await chromium.launch({ headless: true, executablePath: 'C:/Program Files/Google/Chrome/Application/chrome.exe' });
    const context = await browser.newContext({ acceptDownloads: true, viewport: { width: 1440, height: 1000 } });
    const page = await context.newPage();
    page.setDefaultTimeout(10000);
    page.on('pageerror', error => report.errors.push(error.message));
    await page.goto(`http://127.0.0.1:${port}`, { waitUntil: 'networkidle' });
    await page.locator('#auth-user_id').fill('synthetic-reviewer');
    await page.locator('#auth-user_pwd').fill(crypto.randomBytes(24).toString('hex'));
    await page.getByRole('button', { name: 'Login', exact: true }).click();
    await page.locator('.alert-danger').filter({ visible: true }).waitFor();
    report.checks.push('invalid-login-rejected');
    await page.locator('#auth-user_pwd').fill(password);
    await page.getByRole('button', { name: 'Login', exact: true }).click();
    await page.getByRole('link', { name: '데이터 추출', exact: true }).waitFor();
    report.checks.push('authenticated-five-tabs');
    await page.locator('#extracting').click();
    await page.locator('#na\\.table table').waitFor();
    await page.waitForFunction(() => document.getElementById('na.table').innerText.includes('15'));
    await settled(page);
    report.summaryHas15 = (await page.locator('#na\\.table').innerText()).includes('15');
    const pending = page.waitForEvent('download');
    await page.locator('#download\\.basic').click();
    const download = await pending;
    await download.saveAs(path.join(output, 'browser-extraction.xlsx'));
    report.checks.push('extraction-download');
    await page.screenshot({ path: path.join(output, 'synthetic-app.png'), fullPage: true });
    report.visibleErrors = await page.locator('.shiny-output-error:visible').allTextContents();
    report.checks.push('extraction-screen');
    await page.getByRole('link', { name: '개요정보', exact: true }).click();
    await page.locator('#select\\.gaeyo\\.inform').evaluate(element => element.selectize.setValue('주요사업'));
    await page.waitForFunction(() => { const image = document.querySelector('[id="gaeyo.bar"] img'); return image && image.complete && image.naturalWidth > 0; });
    await settled(page);
    const overviewPlot = await page.locator('#gaeyo\\.bar').boundingBox();
    await page.mouse.click(overviewPlot.x + overviewPlot.width * 0.5, overviewPlot.y + overviewPlot.height * 0.5);
    await page.waitForFunction(() => document.getElementById('gaeyo.table').innerText.includes('합성기업'));
    const overviewDownloadWait = page.waitForEvent('download');
    await page.locator('#download\\.gaeyo').click();
    await (await overviewDownloadWait).saveAs(path.join(output, 'browser-summary.xlsx'));
    report.checks.push('overview-click-download');
    await page.getByRole('link', { name: '재무정보', exact: true }).click();
    await page.locator('#select\\.jaemu\\.year').evaluate(element => element.selectize.setValue(['2019', '2020']));
    await page.waitForFunction(() => { const image = document.querySelector('[id="jaemu.hist"] img'); return image && image.complete && image.naturalWidth > 0; });
    await settled(page);
    const financePlot = await page.locator('#jaemu\\.hist').boundingBox();
    await page.mouse.move(financePlot.x + financePlot.width * 0.2, financePlot.y + financePlot.height * 0.5);
    await page.mouse.down();
    await page.mouse.move(financePlot.x + financePlot.width * 0.8, financePlot.y + financePlot.height * 0.5, { steps: 15 });
    await page.mouse.up();
    await page.waitForFunction(() => document.getElementById('data.brush').innerText.includes('합성기업'));
    const financeDownloadWait = page.waitForEvent('download');
    await page.locator('#download\\.jaemu').click();
    await (await financeDownloadWait).saveAs(path.join(output, 'browser-finance.xlsx'));
    report.checks.push('finance-brush-download');
    await page.screenshot({ path: path.join(output, 'synthetic-finance.png'), fullPage: true });
    await page.getByRole('link', { name: '가설검정', exact: true }).click();
    await page.locator('#g1\\.s').evaluate(element => element.selectize.setValue('2019'));
    await page.locator('#g2\\.s').evaluate(element => element.selectize.setValue('2020'));
    await page.waitForFunction(() => document.getElementById('result.test').innerText.includes('Wilcoxon'));
    report.checks.push('paired-test-screen');
    await page.getByRole('link', { name: 'AI 분석', exact: true }).click();
    await page.locator('#ai1').fill('50');
    await page.locator('#ai22').fill('100');
    await page.locator('#start\\.pred').click();
    await page.waitForFunction(() => document.getElementById('before.resulting').innerText.includes('10%'));
    await page.waitForFunction(() => document.getElementById('after.resulting').innerText.includes('90%'));
    await page.locator('#ai\\.selecting').evaluate(element => element.selectize.setValue('도매 및 상품 중개업'));
    await page.waitForFunction(() => document.getElementById('before.resulting').innerText.includes('20%'));
    report.checks.push('prediction-input-and-model-switch');
    await page.getByRole('tab', { name: 'AI 학습정보', exact: true }).click();
    await page.waitForFunction(() => document.getElementById('ai.performance').innerText.includes('학습 데이터 기준'));
    report.checks.push('training-performance-label');
    await page.getByRole('tab', { name: '변수 중요도', exact: true }).click();
    await page.waitForFunction(() => { const image = document.querySelector('[id="ai.variable.importance"] img'); return image && image.complete && image.naturalWidth > 0; });
    report.checks.push('importance-plot');
    await page.screenshot({ path: path.join(output, 'synthetic-prediction.png'), fullPage: true });
    report.visibleErrors = await page.locator('.shiny-output-error:visible').allTextContents();
    if (report.errors.length || report.visibleErrors.length || !report.summaryHas15) throw new Error('브라우저 검증 항목 실패');
  } finally {
    if (browser) await browser.close();
    server.kill();
    report.serverLog = log;
    fs.writeFileSync(path.join(output, 'browser-results.json'), JSON.stringify(report, null, 2));
    console.log(JSON.stringify({ checks: report.checks, errors: report.errors, summaryHas15: report.summaryHas15, visibleErrors: report.visibleErrors }));
  }
}

main().catch(error => { console.error(error.message); process.exitCode = 1; });
