const createIndex = () => {
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

function generateDownloadCommand(e) {
  const titleIndex = createIndex();
  const WEEKNUM = document
    .querySelector('[data-track-component="item_nav_week_number"]')
    .textContent.split(" ")
    .at(1)
    .padStart(2, "0");

  const button = e.target;
  const header = button.previousElementSibling;
  if (header?.tagName !== "H1") {
    throw new Error("Could not find header");
  }

  let title = header.innerText;
  if (!/^\d+.+/.test(title)) {
    DEBUG && console.log(`[INFO] Title does not start with a sequence number.`);
    DEBUG && console.log(`[INFO] Checking index for: '${title}'`);
    if (titleIndex.has(title)) {
      DEBUG && console.log("[INFO] Found matching title in index.");
      title = `${WEEKNUM}-${titleIndex.get(title)}. ${title}`;
    } else {
      DEBUG && console.log("[WARNING] Matching title not found in index");
    }
  }

  const videoDownloadsCSS = 'a[data-track-page="focused_lex_video_item"]';
  const links = Array.from(document.querySelectorAll(videoDownloadsCSS));
  const mp4 = links.filter((e) => /mp4/i.test(e.innerText))[1];
  const pdf = links.filter((e) => /pdf/i.test(e.innerText))[0];
  const vtt = links.filter((e) => /vtt/i.test(e.innerText))[0];
  const txt = links.filter((e) => /txt/i.test(e.innerText))[0];

  let command = [];
  const filename = title.replace(/\//g, "-").replace(/:/g, " -");
  mp4 && command.push('wget -qO "' + filename + '.mp4" "' + mp4.href + '" &');
  pdf && command.push('wget -qO "' + filename + '.pdf" "' + pdf.href + '" &');
  vtt && command.push('wget -qO "' + filename + '.vtt" "' + vtt.href + '" &');
  txt && command.push('wget -qO "' + filename + '.txt" "' + txt.href + '" &');

  return command.join("\n");
}

function handleClick(e) {
  // ensure all lessons are expanded
  document
    .querySelectorAll("h2.lesson-name button[aria-expanded='false']")
    .forEach((e) => e.click());

  const observer = new MutationObserver(async (mutations, obs) => {
    DEBUG && console.log("DOM has been updated after all clicks.");

    // Replace or append to clipboard with generated commands
    const clipboardText = await navigator.clipboard.readText();
    const command = generateDownloadCommand(e);
    const script = /wget/.test(clipboardText)
      ? [clipboardText, command].join("\n")
      : command;
    navigator.clipboard.writeText(script);

    // Advance to next page
    const nextButton = document.querySelector('button[aria-label="Next Item"]');
    if (nextButton) {
      nextButton.click();
    }

    obs.disconnect(); // Stop observing after first batch of changes
  });
  observer.observe(document.body, { childList: true, subtree: true });
}

const toggleDownloads = () => {
  const section = 'button[data-testid="lecture-downloads-tab"]';
  const downloadSection = document.querySelector(section);
  if (downloadSection) downloadSection.click();
};

function addButtonContent() {
    toggleDownloads();
    const btnHtml = `
      <button id="download-content" class="css-1qi5els" type="button">
        Download
      </button>`;
    const headerEl = document.querySelector('h1');
    if (headerEl && !document.getElementById("download-content")) {
      headerEl.insertAdjacentHTML("afterend", btnHtml);
      document
        .getElementById("download-content")
        .addEventListener("click", handleClick);
    }
}
