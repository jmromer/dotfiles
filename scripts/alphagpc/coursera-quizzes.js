/**
 * Convert a single question block into an Org-mode formatted string,
 * preserving the order of paragraphs and tables.
 * @param {Element} questionBlock
 * @returns {string}
 */
function formatQuestionToOrg(questionBlock) {
  if (!questionBlock) return "";

  // 1) Question number and title
  const num = questionBlock.querySelector("h3 span")?.textContent.trim() || "";

  // 2) Grab EVERY <p> and <table> under the viewer in document order
  const viewer = questionBlock.querySelector('[data-testid="cml-viewer"]');
  const contentEls = viewer
    ? Array.from(viewer.querySelectorAll('table, p:not(table p)'))
    : [];

  // 3) Convert each node to Org-mode text
  const contentLines = contentEls.map((el) =>
    el.tagName === "TABLE" ? convertTableToOrg(el) : el.textContent.trim(),
  );

  // 4) First line → intro, rest → body
  const intro = contentLines.shift() || "";
  const body = contentLines.join("\n\n");

  // 5) MCQ options
  const choices = extractAnswerChoices(questionBlock);

  // 6) Assemble
  return (
    [`** ${num}. ${intro}`, body, choices].filter(Boolean).join("\n\n") + "\n"
  );
}

/**
 * Convert an HTML <table> element into an Org-mode table string.
 * @param {HTMLTableElement} tableEl
 * @returns {string}
 */
function convertTableToOrg(tableEl) {
  const rows = Array.from(tableEl.rows);
  const lines = rows.map((r) => {
    const cells = Array.from(r.cells).map((c) => c.textContent.trim());
    return `| ${cells.join(" | ")} |`;
  });

  if (lines.length > 1) {
    const colCount = rows[0].cells.length;
    // build a separator row like "|---+---+---|"
    const sep = "|" + Array(colCount).fill("---+").join("").slice(0, -1) + "|";
    lines.splice(1, 0, sep);
  }

  return lines.join("\n");
}


/**
 * Grab and format MCQ options from the question block.
 * @param {Element} questionBlock
 * @returns {string}
 */
function extractAnswerChoices(questionBlock) {
  const optionEls = questionBlock.parentNode.querySelectorAll(".rc-Option");
  if (!optionEls.length) return "";

  const isMulti = optionEls[0].querySelector("input")?.type === "checkbox";
  const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".split("");
  const list = Array.from(optionEls)
    .map((opt, i) => `- [ ] ${letters[i]}. ${opt.textContent.trim()}`)
    .join("\n");

  return isMulti ? `Select all that apply:\n${list}` : list;
}

/**
 * Collect all questions on the page, convert to Org, copy to clipboard and log.
 */
function exportQuizAsOrg() {
  const blocks = document.querySelectorAll("[data-testid=legend]");
  const doc = Array.from(blocks).map(formatQuestionToOrg).join("\n");
  navigator.clipboard.writeText(doc);
  console.log(doc);
}

/** Insert the "Copy as Org Doc" button and wire up its click. */
function addButtonQuiz() {
  const btnHtml = `
    <button id="copy-org" class="css-1qi5els" type="button">Copy as Org Doc</button>`;
  const headerEl = document.querySelector('[data-testid="header-left"] h1');
  if (headerEl && !document.getElementById("copy-org")) {
    headerEl.insertAdjacentHTML("afterend", btnHtml);
    document
      .getElementById("copy-org")
      .addEventListener("click", exportQuizAsOrg);
  }
}
