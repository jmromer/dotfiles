function gradeTableToAssignmentTSV() {
  function getCourseCode() {
    const title = document.querySelector(
      '[data-e2e="courseNavigation"] h2',
    ).textContent;
    if (title.includes(":")) return title.split(":")[0];
    if (title.includes(".")) return title.split(".")[0];
    if (/^\w+\d+\s+/.test(title)) return title.split(" ")[0];
    if (/^\w+\s\d+\s+/.test(title))
      return title.split(" ").slice(0, 2).join(" ");
    return title
      .split(" ")
      .map((w) => w[0])
      .join("");
  }

  const YEAR = new Date().getFullYear();
  const COURSE = getCourseCode();
  const entries = [];

  const gradesTable = document.querySelectorAll(
    '[aria-label="Assignments Table"] .rc-AssignmentsTableRowCds',
  );
  gradesTable.forEach((row) => {
    const titleLink = row.querySelector("[data-e2e=item-title-text] a");
    const title = titleLink.textContent;
    const href = titleLink.href;
    const [dateEl, timeEl] = row
      .querySelector(".due-column-text")
      .querySelectorAll("div");
    const timestamp = `${dateEl.textContent} ${YEAR} ${timeEl.textContent}`;

    const dueDate = new Date(Date.parse(timestamp));
    if (dueDate.getHours() < 22) {
      dueDate.setHours(0, 0, 0, 0);
      dueDate.setTime(dueDate.getTime() - 1);
    }

    const entry = [
      COURSE,
      null, // week number
      null, // date available
      dueDate.toLocaleDateString(),
      null, // checkbox
      `=HYPERLINK("${href}", "${title}")`,
    ].join("\t");
    entries.push(entry);
  });

  navigator.clipboard.writeText(entries.join("\n"));
  console.log(entries.join("\n"));
}

function addButtonAssignments() {
  const button = `<button id="copy-tsv"
                     class="cds-105 cds-button-disableElevation cds-button-primary css-1qi5els" type="button">
                      <span class="cds-button-label">Copy Assignments TSV</span>
                    </button>`;
  if (document.getElementById("copy-tsv")) return;

  document
    .getElementsByTagName("h1")[0]
    ?.insertAdjacentHTML("afterend", button);

  document.getElementById("copy-tsv")?.addEventListener("click", (e) => {
    gradeTableToAssignmentTSV();
  });
}
