const DEBUG = false;
DEBUG && console.log("alphagpc loading...");

const CONTENT_REGEX = /learn\/.+\/lecture\/.+/;
const QUIZ_REGEX = /(attempt$|view-submission$|view-feedback$)/;
const ASSIGNMENTS_REGEX = /assignments$/;

function setObserver() {
  const maxRuns = 20;
  const runCounts = new Map(); // Track runs per pathname

  const observer = new MutationObserver((_mutations) => {
    DEBUG && console.log("MutationObserver triggered");

    const path = location.pathname;
    const count = runCounts.get(path) || 0;

    if (count >= maxRuns) {
      DEBUG && console.log(`Max run limit (${maxRuns}) reached for ${path}`);
      return;
    }

    runCounts.set(path, count + 1);

    if (CONTENT_REGEX.test(path)) {
      DEBUG && console.log("Content page detected");
      addButtonContent();
    } else if (QUIZ_REGEX.test(path)) {
      DEBUG && console.log("Quiz page detected");
      addButtonQuiz();
    } else if (ASSIGNMENTS_REGEX.test(path)) {
      DEBUG && console.log("Assignments page detected");
      addButtonAssignments();
    }
  });

  observer.observe(document.body, { childList: true, subtree: true });
}

window.addEventListener("load", () => {
  console.log("alphagpc ready.");
  setObserver();
});
