# frozen_string_literal: true

require 'fileutils'
require 'nokogiri'
require 'faraday'

namespace :problem do
  desc "About some euler project's problems"

  task :get, ['index'] do |_, args|
    index = args.index

    dirname = "./problems/p#{index}"
    readme = "README.md"
    pathname = File.join(dirname, readme)

    exit(0) if File.exist?(pathname)

    url = "https://projecteuler.net/problem=#{index}"
    response = Faraday.get(url)
    unless response.status == 200
      STDERR.puts("failed to get project euler #{index}th problem: #{response.status}")
      exit(1)
    end

    doc = Nokogiri::HTML.parse(response.body)
    title = doc.at_css('#content > h2:nth-child(2)')&.text
    problem = doc.at_css('.problem_content')&.text

    if title.nil? or problem.nil?
      STDERR.puts("failed to get project euler #{index}th title or problem")
      exit(1)
    end

    contents = <<~PROBLEM
      [#{title}](#{url})

      #{problem.strip}
    PROBLEM

    FileUtils.mkdir_p(dirname) unless File.exist?(dirname)
    File.open(pathname, 'wt+') do |f|
      f.write(contents)
    end

    sh("git add #{pathname}")
    sh("git commit -m \"Add README of p#{index}\"")
  end
end
