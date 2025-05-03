function quizToOrgMode() {
  function questionText(questionNode, index) {
    const [first, ...rest] = questionNode.querySelectorAll("p");
    const question = first.textContent.split(/\.Question \d+/).join(". ");
    const lines = [`\n** ${index + 1}. ${question}`];
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

    const lines = `\n${options}`;
    if (type === "checkbox") {
      return ["\nSelect all that apply:", lines].join("\n");
    }
    return lines;
  }

  const questions = [...document.querySelectorAll("[data-testid=legend]")]
    .map((e, i) => `${questionText(e, i)}\n${answerChoices(e)}`)
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
