#!/usr/bin/env ruby
# frozen_string_literal: true

Section = Struct.new(:number, :header, :body)

class CompiledMarkdown
  include Enumerable

  def self.from_file(filename)
    section_list =
      File.read(filename).split(/^#\s/).reject(&:empty?).map do |section|
        header, *contents = section.split(/^##\s/).reject(&:empty?)

        contents = contents.first
        section_num, *header = header.split(". ")
        heading = header.join(". ")

        contents = contents.split("\n")
        contents.shift
        contents.shift while contents.first&.empty?
        Section.new(section_num, heading, contents.join("\n").strip)
      end
    new(section_list)
  end

  attr_accessor :sections

  def initialize(section_list)
    self.sections = section_list.to_h { [_1.number, _1] }
  end

  def each(&block)
    sections.each_pair(&block)
  end

  def [](key)
    sections[key]
  end
end
