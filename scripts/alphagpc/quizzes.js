function parseQuestionBlockToOrg(container) {
  if (!container) return '';

  // Get question number
  const questionNumber = container.querySelector('h3 span')?.textContent.trim();

  // Get the question prompt before the table
  const paragraphs = container.querySelectorAll('[data-testid="cml-viewer"] > p');
  let preamble = '';
  if (paragraphs.length > 0) {
    preamble = paragraphs[0].textContent.trim();
  }

  // Get the table and convert to Org mode
  const tableEl = container.querySelector('table');
  let orgTable = '';
  if (tableEl) {
    const rows = Array.from(tableEl.querySelectorAll('tr'));
    const tableLines = rows.map((row, rowIndex) => {
      const cells = Array.from(row.querySelectorAll('td, th'));
      const values = cells.map(cell => cell.textContent.trim());
      return `| ${values.join(' | ')} |`;
    });

    if (tableLines.length > 1) {
      const colCount = tableLines[0].split('|').length - 2;
      const separator = `|${'----------------+'.repeat(colCount).slice(0, -1)}|`;
      tableLines.splice(1, 0, separator);
    }

    orgTable = tableLines.join('\n');
  }

  // Get question continuation (after table)
  let postTableText = '';
  const postTablePara = Array.from(container.querySelectorAll('[data-testid="cml-viewer"] > p')).slice(1);
  postTableText = postTablePara.map(p => p.textContent.trim()).join('\n\n');

  // Get MCQ choices if any
  const choices = answerChoices(container)

  // Combine all parts
  const question = [preamble, orgTable, postTableText, choices].filter(e => e.length).join("\n\n");
  return `** ${questionNumber}. ${question}\n`;
}

function questionText(questionNode, index) {
  const [first, ...rest] = questionNode.querySelectorAll("p");
  const question = first.textContent.split(/\.Question \d+/).join(". ");
  const lines = [`** ${index + 1}. ${question}`];
  if (rest.length) {
    let body = rest.map((e) => e.textContent).join("\n");
    lines.push(`\n${body}`);
  }
  return lines.join("\n");
}

function answerChoices(questionNode) {
  const letters = "ABCDEFGHIJKLMNOPQ".split("");
  const container = questionNode.parentNode;
  const optionNodes = container.querySelectorAll(".rc-Option");
  const type = optionNodes[0].querySelector("input").type;
  const options = [...optionNodes]
    .map((o, i) => `- [ ] ${letters[i]}. ${o.textContent}`)
    .join("\n");

  if (type === "checkbox") {
    return ["Select all that apply:", options].join("\n");
  }
  return options;
}

function quizToOrgMode() {
  const questions =
        [...document.querySelectorAll("[data-testid=legend]")]
        .map((e, i) => parseQuestionBlockToOrg(e))
        .join("\n");
  navigator.clipboard.writeText(questions);
  console.log(questions);
}

const button = `<button
  id="copy-org"
  class="cds-105 cds-button-disableElevation cds-button-primary css-1qi5els" type="button">
  <span class="cds-button-label">
    Copy as Org Doc
  </span>
</button>`;

let count = 0;
const observer = new MutationObserver((_mutations) => {
  if (count++ > 10 || document.getElementById("copy-org")) return;
  const header = document.querySelector('[data-testid="header-left"] h1');
  header?.insertAdjacentHTML("afterend", button);
  document.getElementById("copy-org")?.addEventListener("click", (e) => {
    quizToOrgMode();
  });
});
observer.observe(document.body, { childList: true, subtree: true });
