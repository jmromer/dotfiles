const DEBUG = false;
DEBUG && console.log("alphagpc loading...");

const CONTENT_REGEX = /learn\/.+\/lecture\/.+/;
const QUIZ_REGEX = /(attempt$|view-submission$|view-feedback$)/;
const ASSIGNMENTS_REGEX = /assignments$/;

window.addEventListener("load", () => {
  console.log("alphagpc ready.");
  const observer = new MutationObserver((_mutations) => {
    if (CONTENT_REGEX.test(location.pathname)) {
      DEBUG && console.log("Content page detected");
      addButtonContent();
    } else if (QUIZ_REGEX.test(location.pathname)) {
      DEBUG && console.log("Quiz page detected");
      addButtonQuiz();
    } else if (ASSIGNMENTS_REGEX.test(location.pathname)) {
      DEBUG && console.log("Assignments page detected");
      addButtonAssignments();
    }
  });
  observer.observe(document.body, { childList: true, subtree: true });
});
