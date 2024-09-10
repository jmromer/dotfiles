function restoreNativeLinks() {
  const baseUrl = window.location.href.replace(/feed.+/, "feed");
  const tags = document.querySelectorAll(".tag:not(.tag--link)");
  if (!tags.length) return;

  tags.forEach((e) => {
    const clone = e.cloneNode(true);
    e.parentNode.replaceChild(clone, e);

    const href = `${baseUrl}/${clone.textContent.replace("#", "")}`;
    clone.classList.add("tag--link");
    clone.setAttribute("href", href);
    clone.setAttribute("target", "_blank");
  });
}

window.addEventListener("load", restoreNativeLinks);
const observer = new MutationObserver(restoreNativeLinks);
observer.observe(document.body, { childList: true, subtree: true });

function createBookmark({ target }) {
  if (["fa-link", "icon-btn"].some((e) => target.classList.contains(e))) {
    const button = target.closest("button");
    button.setAttribute("title", "Create bookmark");

    const url = button.getAttribute("data-clipboard-text");
    const number = url.split("/").pop();
    const title = `PSL #${number}. ${document.querySelector("h3").textContent}`;

    const bookmark = { parentId: "1", title, url };
    chrome.runtime.sendMessage({ bookmark }, (response) => {});
  }
}
window.addEventListener("click", createBookmark);

console.log("[crampuswire] loaded ✅");
