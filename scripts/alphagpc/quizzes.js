/**
 * Convert a single question block into an Org-mode formatted string.
 * @param {Element} questionBlock
 * @returns {string}
 */
function formatQuestionToOrg(questionBlock) {
  if (!questionBlock) return '';

  const num = questionBlock.querySelector('h3 span')?.textContent.trim();
  const paragraphs = questionBlock.querySelectorAll('[data-testid="cml-viewer"] > p');
  const intro = paragraphs[0]?.textContent.trim() || '';

  // Convert HTML table to Org table
  const tableEl = questionBlock.querySelector('table');
  let orgTable = '';
  if (tableEl) {
    const rows = Array.from(tableEl.rows);
    const lines = rows.map(r => {
      const cells = Array.from(r.cells).map(c => c.textContent.trim());
      return `| ${cells.join(' | ')} |`;
    });
    if (lines.length > 1) {
      const colCount = rows[0].cells.length;
      // e.g. |---+---+---|
      const sep = '|' + Array(colCount).fill('---+').join('').slice(0, -1) + '|';
      lines.splice(1, 0, sep);
    }
    orgTable = lines.join('\n');
  }

  // Any paragraphs after the table
  const continuation = Array.from(paragraphs)
    .slice(1)
    .map(p => p.textContent.trim())
    .join('\n\n');

  // Multiple-choice options
  const choices = extractAnswerChoices(questionBlock);

  return [
    `** ${num}. ${intro}`,
    orgTable,
    continuation,
    choices
  ].filter(Boolean).join('\n\n') + '\n';
}

/**
 * Grab and format MCQ options from the question block.
 * @param {Element} questionBlock
 * @returns {string}
 */
function extractAnswerChoices(questionBlock) {
  const optionEls = questionBlock.parentNode.querySelectorAll('.rc-Option');
  if (!optionEls.length) return '';

  const isMulti = optionEls[0].querySelector('input')?.type === 'checkbox';
  const letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('');
  const list = Array.from(optionEls)
    .map((opt, i) => `- [ ] ${letters[i]}. ${opt.textContent.trim()}`)
    .join('\n');

  return isMulti
    ? `Select all that apply:\n${list}`
    : list;
}

/**
 * Collect all questions on the page, convert to Org, copy to clipboard and log.
 */
function exportQuizAsOrg() {
  const blocks = document.querySelectorAll('[data-testid=legend]');
  const doc = Array.from(blocks)
    .map(formatQuestionToOrg)
    .join('\n');
  navigator.clipboard.writeText(doc);
  console.log(doc);
}

/** Insert the “Copy as Org Doc” button and wire up its click. */
function insertCopyButton() {
  const btnHtml = `
    <button id="copy-org" class="css-1qi5els" type="button">Copy as Org Doc</button>`;
  const headerEl = document.querySelector('[data-testid="header-left"] h1');
  if (headerEl && !document.getElementById('copy-org')) {
    headerEl.insertAdjacentHTML('afterend', btnHtml);
    document.getElementById('copy-org')
      .addEventListener('click', exportQuizAsOrg);
  }
}

// Watch for when the header appears, then add the button once and disconnect.
(function setupButtonObserver() {
  const observer = new MutationObserver(() => {
    insertCopyButton();
    if (document.getElementById('copy-org')) {
      observer.disconnect();
    }
  });
  observer.observe(document.body, { childList: true, subtree: true });
})();
