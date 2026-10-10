#!/usr/bin/env ruby

require 'net/http'
require 'json'
require 'uri'

# Load .env file if it exists
if File.exist?('.env')
  File.readlines('.env').each do |line|
    line.strip!
    next if line.empty? || line.start_with?('#')
    key, value = line.split('=', 2)
    ENV[key.strip] = value.strip.sub(/\A['"]?(.*?)['"]?\z/, '\1') if key && value
  end
end

JIRA_SITE = ENV.fetch('JIRA_SITE', 'https://reload.atlassian.net')
EMAIL = ENV['JIRA_EMAIL']
API_TOKEN = ENV['JIRA_API_TOKEN']

unless EMAIL && API_TOKEN
  abort "Error: Please set JIRA_EMAIL and JIRA_API_TOKEN environment variables."
end

# JQL to get issues assigned to the current user (ignoring Done issues, ordered by recently updated)
jql = URI.encode_www_form_component('assignee = currentUser() AND statusCategory != Done ORDER BY updated DESC')
uri = URI.parse("#{JIRA_SITE}/rest/api/3/search/jql?jql=#{jql}&fields=key,summary,status")

request = Net::HTTP::Get.new(uri)
request.basic_auth(EMAIL, API_TOKEN)
request['Accept'] = 'application/json'

req_options = {
  use_ssl: uri.scheme == 'https'
}

response = Net::HTTP.start(uri.hostname, uri.port, req_options) do |http|
  http.request(request)
end

if response.is_a?(Net::HTTPSuccess)
  data = JSON.parse(response.body)
  issues = data['issues'] || []
  
  if !issues.empty?
    issues.each do |issue|
      key = issue['key']
      summary = issue.dig('fields', 'summary')
      status = issue.dig('fields', 'status', 'name')
      
      puts "#{key} #{summary} (#{status})"
    end
  end
else
  abort "Failed to fetch tickets. HTTP #{response.code}: #{response.body}"
end
