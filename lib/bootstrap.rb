#!/usr/bin/env ruby
# frozen_string_literal: true

unless ENV["BOOTSTRAP_DEBUG"].to_s.empty?
  def system(command); end
end

def install_homebrew
  shell(`curl -sL brew.sh | grep install.sh | sed -E 's/<[^>]*>//g'`.strip)
end

def brew_install(package, version: nil, options: [])
  package_name = (version ? "#{package}@#{version}" : package.to_s).tr("_", "-")
  flags = Array(options).map { |opt| "--#{opt.to_s.tr("_", "-")}" }

  shell("brew install #{package_name} #{flags.join(" ")}")
end

def cask_install(package_name, options: [])
  brew_install(package_name, options: options.push(:cask))
end

def asdf_install
  shell("asdf install")
end

def brew_tap(tap_name)
  shell("brew tap #{tap_name}")
end

def shell(command)
  puts(command)
  system(command)
end

def get_yes_no(prompt = "")
  loop do
    print [prompt, "[Y/n]: "].reject(&:empty?).join(" ")

    case gets.strip
    when /^(yes|y)$/i, "" then return true
    when /^(no|n)$/i      then return false
    end
  end
end

def group(group_name)
  puts "\n",
       "Install #{group_name.to_s.tr("_", " ")}?",
       "---------------------------------"

  if get_yes_no
    yield if block_given?
  else
    puts "Skipping."
  end
end

def prompt_and_wait(prompt)
  puts prompt
  loop { break if get_yes_no("Continue?") }
end

def install_quicklook_viewers
  shell("xattr -d -r com.apple.quarantine ~/Library/QuickLook")
  shell("qlmanage -r")
end

def link_launchagent(filename)
  shell("ln -sfv ~/.dotfiles/launchagents/#{filename}.plist ~/Library/LaunchAgents/")
end
