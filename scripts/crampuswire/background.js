chrome.runtime.onMessage.addListener(({ bookmark }, sender, sendResponse) => {
  chrome.bookmarks.create(bookmark, (bookmark) => {});
});
