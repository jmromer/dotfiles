const COURSE_SELECTOR = ".module__short-name";
const MODULE_SELECTOR = ".context_module[aria-label*=Week]";
const ITEM_SELECTOR = ".with-completion-requirements";
const TITLE_SELECTOR = ".module-item-title";
const SCHEDULE_ITEM_SELECTOR = ".ig-info:has(.points_possible_display) .module-item-title";
const DETAILS_SELECTOR = ".ig-details";

function getCourseCode() {
  const course = document
    .querySelector(COURSE_SELECTOR)
    .textContent.split("_")[0];
  return course.split(/(?<=\D+)(?=\d+)/).join(" ");
}

function getScheduleAssignments(moduleEl) {
  const modules = [...moduleEl.querySelectorAll(SCHEDULE_ITEM_SELECTOR)];
  return modules
    .map((e) => e.textContent.trim().split("\n")[0])
    .filter((e) => !/Watch:/.test(e))
    .map((e) => e.split(":")[0])
    .join(", ");
}

function getSchedule() {
  const modules = Array.from(document.querySelectorAll(MODULE_SELECTOR));
  return modules.map((m) => {
    const weekNum = getWeekNumber(m);
    const topic = m.textContent.trim().split(/\s-\s|\n/)[1];
    return [weekNum, topic, getScheduleAssignments(m)].join("\t");
  });
}

function getWeekNumber(moduleEl) {
  const header = moduleEl.textContent.trim().split(" - ")[0];
  const number = new Map(
    [...header.matchAll(/(Module|Week) (\d+)/g)].map((e) => e.slice(1, 3)),
  );
  const weekNum = number.get("Week");
  return +weekNum - 1;
}

function formatTitle(itemEl) {
  const title = itemEl
    .querySelector(TITLE_SELECTOR)
    .textContent.trim()
    .split("\n")[0];
  return title.replace(/ Assignment$/, "");
}

function parseDueDate(itemEl) {
  const textLine = itemEl
    .querySelector(DETAILS_SELECTOR)
    .textContent.trim()
    .split("\n")[0];
  const ms = Date.parse(textLine);
  if (isNaN(ms)) return "";
  return new Date(ms).toISOString().split("T")[0];
}

function getAssignments() {
  const courseCode = getCourseCode();
  const modules = Array.from(document.querySelectorAll(MODULE_SELECTOR));

  return modules.flatMap((moduleEl) => {
    const weekNum = getWeekNumber(moduleEl);
    const items = Array.from(moduleEl.querySelectorAll(ITEM_SELECTOR));
    const entries = items.map((itemEl) => [parseDueDate(itemEl), formatTitle(itemEl)]);

    // Course	Week	Available	Due EOD		Assignment
    return entries.map(([date, title], _, all) => {
      const dueDate = date || all.map((e) => e[0]).filter((e) => e).toSorted()[0];
      return `${courseCode}\t${weekNum}\t\t${dueDate}\t\t${title}`;
    });
  });
}

function expandAll() {
  const collapsed = document.getElementById("context_modules").querySelectorAll('.collapsed_module');
  for (const module of collapsed) {
    module.querySelector(".collapse_module_link").click();
  }
}

function parseAssignments() {
  expandAll()
  const assignments = getAssignments().join("\n");
  navigator.clipboard.writeText(assignments);
  console.log(assignments);
  setInterval(() => {
    window.scrollTo({ top: 0, left: 0, behavior: "smooth" });
  }, 1000);
}

function parseSchedule() {
  expandAll()
  const schedule = getSchedule().join("\n");
  navigator.clipboard.writeText(schedule);
  console.log(schedule);
  setInterval(() => {
    window.scrollTo({ top: 0, left: 0, behavior: "smooth" });
  }, 1000);
}

const buttons = `
<button id="copy-schedule" class="" type="button">
  <span class="cds-button-label">Copy Schedule</span>
</button>
<button id="copy-assignments" class="" type="button">
  <span class="cds-button-label">Copy Assignments</span>
</button>`;
document.getElementsByTagName("h1")[0]?.insertAdjacentHTML("afterEnd", buttons);
document.getElementById("copy-schedule")?.addEventListener("click", parseSchedule);
document.getElementById("copy-assignments")?.addEventListener("click", parseAssignments);
