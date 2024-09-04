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
    .map((e, i) => [e, (i + 1).toString().padStart(2, "0")]);
  return new Map(sequenceNumToItem);
};

function handleClick(e) {
  const titleIndex = createIndex();

  let title = e.target.innerText;
  if (!/^\d+.+/.test(title)) {
    console.log(`[INFO] Title does not start with a sequence number.`);
    console.log(`[INFO] Checking index for: '${title}'`);
    if (titleIndex.has(title)) {
      console.log("[INFO] Found matching title in index.");
      title = `${titleIndex.get(title)}. ${title}`;
    } else {
      console.log("[WARNING] Matching title not found in index");
    }
  }

  const videoDownloadsCSS = 'a[data-track-page="focused_lex_video_item"]';
  const links = Array.from(document.querySelectorAll(videoDownloadsCSS));
  const mp4 = links.filter((e) => /mp4/i.test(e.innerText))[0];
  const pdf = links.filter((e) => /pdf/i.test(e.innerText))[0];
  const vtt = links.filter((e) => /vtt/i.test(e.innerText))[0];
  const txt = links.filter((e) => /txt/i.test(e.innerText))[0];

  let command = [];
  const filename = title.replace(/[\/:]/g, "_");
  mp4 && command.push('wget -O "' + filename + '.mp4" "' + mp4.href + '"');
  pdf && command.push('wget -O "' + filename + '.pdf" "' + pdf.href + '"');
  vtt && command.push('wget -O "' + filename + '.vtt" "' + vtt.href + '"');
  txt && command.push('wget -O "' + filename + '.txt" "' + txt.href + '"');
  command = command.join("\n");

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
