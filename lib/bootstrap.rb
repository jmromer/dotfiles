#!/usr/bin/env ruby
# frozen_string_literal: true

require "set"

IN_DEBUG_MODE = !ENV["BOOTSTRAP_DEBUG"].to_s.empty?

if IN_DEBUG_MODE
  puts "-*- Running in debug mode -*-"

  def system(*args); end
end

DOTFILES_DIR = File.expand_path("~/.dotfiles")

ASDF_PLUGINS =
  File
    .readlines("#{DOTFILES_DIR}/config/asdf/tool-versions")
    .map(&:split)
    .map(&:first)
    .freeze

ENVIRONMENT =
  %x[
    zsh -c "
      . #{DOTFILES_DIR}/config/zsh/.zshenv &&
      . #{DOTFILES_DIR}/env/xdg.sh &&
      . #{DOTFILES_DIR}/env/asdf.sh &&
      env |
      grep -Ei '(xdg|asdf|path)'"
  ].strip
   .split("\n")
   .map { |line| line.split("=") }
   .push(["HOMEBREW_NO_INSTALL_CLEANUP", "true"])
   .sort_by(&:first)
   .to_h
   .freeze
   .tap { |env| puts "-" * 60, env.map { |e| e.join("=") }, "-" * 60 if IN_DEBUG_MODE }

INSTALLED_FORMULAS =
  %x[brew list -1 --formula].strip.split("\n").to_set.freeze

INSTALLED_CASKS =
  %x[brew list -1 --cask].strip.split("\n").to_set.freeze

INSTALLED_TAPS =
  %x[brew tap].strip.split("\n").to_set.freeze

def install_homebrew
  if %x[type brew 2>/dev/null].strip.empty?
    execho(`curl -sL brew.sh | grep install.sh | sed -E 's/<[^>]*>//g'`.strip)
  else
    puts "Homebrew already installed. Skipping."
  end
end

def brew_install(package, version: nil, cask: false, options: [])
  package_name = (version ? "#{package}@#{version}" : package.to_s).tr("_", "-")

  flags =
    Array(options)
      .map { |opt| "--#{opt.to_s.tr("_", "-")}" }
      .tap { |flags| flags << "--cask" if cask }

  if brew_already_installed?(package_name, cask: cask)
    puts "#{package_name} already installed. Skipping."
  else
    execho("brew install #{package_name} #{flags.join(" ")}")
  end
end

def brew_already_installed?(package_name, cask: false)
  if cask
    INSTALLED_CASKS.include?(package_name)
  else
    INSTALLED_FORMULAS.include?(package_name)
  end
end

def cask_install(package_name, options: [])
  brew_install(package_name, cask: true, options: options)
end

def brew_cleanup
  execho("brew cleanup")
end

def brew_doctor
  execho("brew doctor")
end

def asdf_install_plugins
  ASDF_PLUGINS.each { |plugin| execho("asdf plugin-add #{plugin}") }
end

def asdf_install_versions
  ASDF_PLUGINS.each { |plugin| execho("asdf install #{plugin}") }
end

def brew_tap(tap_name)
  if INSTALLED_TAPS.include?(tap_name.to_s)
    puts("#{tap_name} already tapped. Skipping")
  else
    execho("brew tap #{tap_name}")
  end
end

def execho(command)
  puts(command)
  system(ENVIRONMENT, command)
end

def get_yes_no(prompt = "")
  loop do
    print [prompt, "[Y/n]: "].reject(&:empty?).join(" ")

    case gets.strip
    when /^(yes|y)$/i, "" then return true
    when /^(no|n)$/i then return false
    end
  end
end

def setup(group_name)
  gated_prompt("Set up", group_name) { yield if block_given? }
end

def configure(group_name)
  gated_prompt("Configure", group_name) { yield if block_given? }
end

def perform(group_name)
  gated_prompt("Perform", group_name) { yield if block_given? }
end

def gated_prompt(message, group_name)
  prompt = "#{message} #{group_name.to_s.tr("_", " ")}?"
  puts "\n", prompt, "-" * prompt.length

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
  execho("xattr -d -r com.apple.quarantine ~/Library/QuickLook")
  execho("qlmanage -r")
end

def install_launchagent(filename)
  execho("ln -sfv #{DOTFILES_DIR}/launch_agents/#{filename}.plist ~/Library/LaunchAgents")
  execho("launchctl load ~/Library/LaunchAgents/#{filename}.plist")
end
