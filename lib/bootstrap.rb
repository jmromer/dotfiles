#!/usr/bin/env ruby
# frozen_string_literal: true

require "set"
require "fileutils"

# Utility commands
# ----------------

def prompt_and_wait(prompt)
  puts prompt
  loop { break if get_yes_no("Continue?") }
end

def setup(group_name, default: true)
  gated_prompt("Set up", group_name, default: default) { yield if block_given? }
end

def setup_writing(project)
  FileUtils.mkdir_p(File.expand_path("~/Writing/#{project}"))
  execho("git clone --recursive -j2 https://github.com/jmromer/#{project}.git ~/Writing/#{project}")
end

def configure(group_name)
  gated_prompt("Configure", group_name) { yield if block_given? }
end

def perform(group_name)
  gated_prompt("Perform", group_name) { yield if block_given? }
end

def gated_prompt(message, group_name, default: true)
  prompt = "#{message} #{humanized group_name}?"
  puts "\n", prompt, "-" * prompt.length

  if get_yes_no(default: default)
    yield if block_given?
  else
    puts "Skipping."
  end
end

def get_yes_no(prompt = "", default: true)
  options = default ? "[Y/n]: " : "[y/N]: "
  loop do
    print [prompt, options].reject(&:empty?).join(" ")

    case gets.to_s.strip
    when /^(yes|y)$/i then return true
    when /^(no|n)$/i then return false
    when "" then return default
    end
  end
rescue Interrupt
  exit 0
end

def execho(command)
  puts(command)
  system(ENVIRONMENT, command)
end

def humanized(str)
  str.to_s.tr("_", " ")
end

def dashified(str)
  str.to_s.gsub(/[_\s]/, "-")
end

def flagified(str)
  "--#{dashified str}"
end

def machine_is?(machine_type)
  arch = `uname -ps`
  case machine_type
  when :apple_silicon then arch.match?(/^Darwin arm/)
  when :mac, :intel_silicon then arch.match?(/^Darwin/)
  when :linux then arch.match?(/^Linux/)
  end
end

# Xcode-related
# -------------

def ensure_xcode_is_set_up
  unless xcode_installed?
    puts "Please install Xcode before running this script."
    abort
  end

  xcode_configure
end

def xcode_installed?
  system("test -d /Applications/Xcode.app")
end

def xcode_configured?
  system("xcode-select --print-path | grep -q '^/Applications/Xcode'")
end

def xcode_configure
  return if xcode_configured?

  puts("Enter your password to configure Xcode CLI tools.")
  system("sudo xcode-select --switch /Applications/Xcode.app")
end

# Shell commands
# --------------

def install_launchagent(filename)
  FileUtils.mkdir_p(File.expand_path("~/Library/LaunchAgents/"))
  execho("ln -sfv ${DOTFILES_DIR}/launch_agents/#{filename}.plist ~/Library/LaunchAgents/#{filename}.plist")
  execho("launchctl unload ~/Library/LaunchAgents/#{filename}.plist 2>/dev/null")
  execho("launchctl load ~/Library/LaunchAgents/#{filename}.plist")
end

def reset_quicklookd
  execho("qlmanage -r")
end

def script_install(name)
  full_path = "#{DOTFILES_DIR}/lib/install-#{name}"

  if command_exists?(full_path)
    execho(full_path)
  else
    puts "Error: install script for #{name} does not exist or is not executable."
  end
end

def command_exists?(command)
  system("command -v #{command} >/dev/null 2>&1")
end

def ensure_gpg_permissions_are_set_correctly
  system(ENVIRONMENT, "chmod 600 ${XDG_SECURE_DIR}/gnupg/*")
  system(ENVIRONMENT, "chmod 700 ${XDG_SECURE_DIR}/gnupg")
end

def ensure_locals_are_created
  system(ENVIRONMENT, "mkdir -p ${XDG_LOCALS_DIR}/bin")
  system(ENVIRONMENT, "mkdir -p ${XDG_LOCALS_DIR}/config")
  return unless machine_is?(:mac)

  system(ENVIRONMENT, "ln -sf ${HOMEBREW_PREFIX}/bin/pinentry-mac ${XDG_LOCALS_DIR}/bin/")
end

# Homebrew-related commands
# -------------------------

def install_homebrew
  if command_exists?(:brew)
    puts "Homebrew already installed. Skipping."
  else
    execho(`curl -sL brew.sh | grep install.sh | sed -E 's/<[^>]*>//g'`.strip)
  end
end

def brew_tap(tap_name)
  if brew_installed?(tap_name)
    puts("#{tap_name} already tapped. Skipping")
  else
    execho("brew tap #{tap_name}")
  end
end

def brew_install(package, version: nil, options: [])
  package_name = version ? "#{package}@#{version}" : package
  package_name = dashified(package_name) if package.is_a?(Symbol)

  flags = Array(options).map { |option| flagified option }.join(" ")
  is_cask = Array(options).include?(:cask)

  if brew_installed?(package_name, type: is_cask ? :cask : :formula)
    puts "#{package_name} already installed. Skipping."
  else
    execho("brew install #{package_name} #{flags}")
  end
end

def cask_install(package_name, version: nil, options: [])
  brew_install(package_name, version: version, options: [:cask, *options])
end

def brew_cleanup
  execho("brew cleanup")
end

def brew_doctor
  execho("brew doctor")
end

def brew_installed?(name, type: :formula)
  case type
  when :tap
    BREW_INSTALLED_TAPS.include?(name.to_s)
  when :formula
    BREW_INSTALLED_FORMULAS.include?(name.to_s)
  when :cask
    BREW_INSTALLED_CASKS.include?(name.to_s)
  end
end

# Help
# ----------
if ARGV.delete("--help").to_s.eql?("--help")
  puts "Run with --debug to dry-run."
  exit 2
end

# Debug mode
# ----------
if (IN_DEBUG_MODE = ARGV.delete("--debug").to_s.eql?("--debug"))
  puts "-*- Running in debug mode -*-"
  def execho(*args); system(ENVIRONMENT, *args); end
  def system(*args); puts(*args); true; end
end

# Constants
# ---------

DOTFILES_DIR = File.dirname(File.dirname(__FILE__))
HOMEBREW_PREFIX = '/opt/homebrew'

ENVIRONMENT =
  `zsh -c ". #{DOTFILES_DIR}/config/zsh/.zshenv && env | grep -E '^(XDG_|MISE_|PATH)'"`
  .strip
  .split("\n")
  .map { |line| line.split("=") }
  .push(["HOMEBREW_NO_INSTALL_CLEANUP", true.to_s])
  .push(["HOMEBREW_PREFIX", HOMEBREW_PREFIX])
  .push(["DOTFILES_DIR", DOTFILES_DIR])
  .sort_by(&:first)
  .to_h
  .freeze
  .tap { |env| puts "-" * 60, env.map { |e| e.join("=") }, "-" * 60 if IN_DEBUG_MODE }

BREW_INSTALLED_FORMULAS =
  `brew list -1 --formula 2>/dev/null`.strip.split("\n").to_set.freeze

BREW_INSTALLED_CASKS =
  `brew list -1 --cask 2>/dev/null`.strip.split("\n").to_set.freeze

BREW_INSTALLED_TAPS =
  `brew tap 2>/dev/null`.strip.split("\n").to_set.freeze
