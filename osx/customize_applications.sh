#-------------------------------------------------------------
# App customizations
#-------------------------------------------------------------
SYNCED_PREFERENCES="$HOME/Dropbox/Sync/Preferences"

fecho "Linking to Sublime Text 3 User Profile"
rm -rf "$HOME/Library/Application Support/Sublime Text 3/Packages/User"
ln -s $SYNCED_PREFERENCES/SublimeText_Packages_User "$HOME/Library/Application Support/Sublime Text 3/Packages/User"

fecho "Run the following in the ST3 console to install Package Manager"
echo "import urllib.request,os,hashlib; h = '7183a2d3e96f11eeadd761d777e62404' + 'e330c659d4bb41d3bdf022e94cab3cd0'; pf = 'Package Control.sublime-package'; ipp = sublime.installed_packages_path(); urllib.request.install_opener( urllib.request.build_opener( urllib.request.ProxyHandler()) ); by = urllib.request.urlopen( 'http://sublime.wbond.net/' + pf.replace(' ', '%20')).read(); dh = hashlib.sha256(by).hexdigest(); print('Error validating download (got %s instead of %s), please try manual install' % (dh, h)) if dh != h else open(os.path.join( ipp, pf), 'wb' ).write(by)"

fecho "Linking to iTerm2 User Preferences"
ln -sf $SYNCED_PREFERENCES/com.googlecode.iterm2.plist $HOME/Library/Preferences/com.googlecode.iterm2.plist

fecho "Linking to Vim User Profile"
rm -rf $HOME/.vim
ln -s $SYNCED_PREFERENCES/vim $HOME/.vim

fecho "Linking to Firefox User Profile"
rm -rf "$HOME/Library/Application Support/Firefox"
ln -s $SYNCED_PREFERENCES/Firefox/ "$HOME/Library/Application Support/Firefox"

fecho "Linking to Stickies Database"
rm -f $HOME/Library/StickiesDatabase
ln -s $SYNCED_PREFERENCES/StickiesDatabase $HOME/Library/StickiesDatabase

fecho "Linking to Stickies Database"
rm -rf "$HOME/Library/Application Support/Papers3"
ln -s $SYNCED_PREFERENCES/Papers3 "$HOME/Library/Application Support/Papers3"

fecho "Linking to Jitouch preference list"
ln -fs $SYNCED_PREFERENCES/com.jitouch.Jitouch.plist $HOME/Library/Preferences/com.jitouch.Jitouch.plist

