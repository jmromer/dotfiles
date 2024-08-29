console.log("alphagpc loading...");

const addListener = () => {
  document.querySelector("h1").addEventListener("click", (e) => {
    const title = e.target.innerText.replace(/:/g, "");
    const links = Array.from(
      document.querySelectorAll('a[data-track-page="focused_lex_video_item"]'),
    );
    const mp4 = links.filter((e) => /mp4/i.test(e.innerText))[0];
    const pdf = links.filter((e) => /pdf/i.test(e.innerText))[0];
    const vtt = links.filter((e) => /vtt/i.test(e.innerText))[0];
    const text = [
      'wget -O "' + title + '.mp4" "' + mp4.href + '"',
      'wget -O "' + title + '.pdf" "' + pdf.href + '"',
      'wget -O "' + title + '.vtt" "' + vtt.href + '"',
    ].join("\n");
    navigator.clipboard.writeText(text);
    console.log(text);
  });
};

const toggleDownloads = () => {
  const section =
    'button[data-track-component="focused_lex_lecture_tabs_download"]';
  const downloadSection = document.querySelector(section);
  if (downloadSection) downloadSection.click();
};

window.addEventListener("load", () => {
  console.log("alphagpc ready.");
  toggleDownloads();
  addListener();
});

const observer = new MutationObserver((_mutations) => {
  console.log("DOM mutation detected");
  toggleDownloads();
  addListener();
});

observer.observe(document.body, { childList: true, subtree: true });
