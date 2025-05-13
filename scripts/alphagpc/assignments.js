function gradeTableToAssignmentCSV() {
  function getCourseCode() {
    const title = document.querySelector(
      '[data-e2e="courseNavigation"] h2',
    ).textContent;
    if (title.includes(":")) return title.split(":")[0];
    if (title.includes(".")) return title.split(".")[0];
    if ((/^\w+\d+\s+/).test(title)) return title.split(" ")[0];
    if ((/^\w+\s\d+\s+/).test(title)) return title.split(" ").slice(0, 2).join(" ");
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
      title,
      href,
    ].join(",");
    entries.push(entry);
  });

  navigator.clipboard.writeText(entries.join("\n"));
  console.log(entries.join("\n"));
}

const button = `<button id="copy-csv"
  class="cds-105 cds-button-disableElevation cds-button-primary css-1qi5els" type="button">
  <span class="cds-button-label">
    Copy Assignments CSV
  </span>
</button>`;

let count = 0;
const observer = new MutationObserver((_mutations) => {
  if (count++ > 10 || document.getElementById("copy-csv")) return;
  document
    .getElementsByTagName("h1")[0]
    ?.insertAdjacentHTML("afterend", button);
  document.getElementById("copy-csv")?.addEventListener("click", (e) => {
    gradeTableToAssignmentCSV();
  });
});
observer.observe(document.body, { childList: true, subtree: true });
