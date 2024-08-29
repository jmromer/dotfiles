console.log("alphagpc loading...");

const createIndex = () => {
  // ensure all lessons are expanded
  document
    .querySelectorAll("h2.lesson-name button[aria-expanded='false']")
    .forEach((e) => e.click());

  // get all lesson title elements
  const navItems = document
    .querySelector(".item-tools-and-content-container")
    .querySelectorAll("a .rc-NavItemName");

  // create a map of lesson titles to sequence numbers
  const sequenceNumToItem = Array.from(navItems)
    .filter((e) => /video/i.test(e.childNodes[0].textContent))
    .map((e) => e.childNodes[e.childNodes.length - 1].textContent)
    .map((e, i) => [e, i.toString().padStart(2, "0")]);
  return new Map(sequenceNumToItem);
};

function handleClick(e) {
  const titleIndex = createIndex();

  let title = e.target.innerText.replace(/:/g, "");
  if (!/^\d+.+/.test(title)) {
    title = `${titleIndex.get(title)}. ${title}`;
    if (!titleIndex.has(title))
      console.log("[WARNING] Matching title not found in index");
  }

  const links = Array.from(
    document.querySelectorAll('a[data-track-page="focused_lex_video_item"]'),
  );
  const mp4 = links.filter((e) => /mp4/i.test(e.innerText))[0];
  const pdf = links.filter((e) => /pdf/i.test(e.innerText))[0];
  const vtt = links.filter((e) => /vtt/i.test(e.innerText))[0];
  const command = [
    'wget -O "' + title + '.mp4" "' + mp4.href + '"',
    'wget -O "' + title + '.pdf" "' + pdf.href + '"',
    'wget -O "' + title + '.vtt" "' + vtt.href + '"',
  ].join("\n");
  navigator.clipboard.writeText(command);
  console.log(command);
}

const toggleDownloads = () => {
  const section =
    'button[data-track-component="focused_lex_lecture_tabs_download"]';
  const downloadSection = document.querySelector(section);
  if (downloadSection) downloadSection.click();
};

// TODO: prevent duplicate event listeners
window.addEventListener("load", () => {
  console.log("alphagpc ready.");
  toggleDownloads();
  document.querySelector("h1").addEventListener("click", handleClick);
  document.querySelector("h1").setAttribute("style", "cursor: pointer;");
});

const observer = new MutationObserver((_mutations) => {
  console.log("DOM mutation detected");
  toggleDownloads();
  document.querySelector("h1").addEventListener("click", handleClick);
  document.querySelector("h1").setAttribute("style", "cursor: pointer;");
});
observer.observe(document.body, { childList: true, subtree: true });
