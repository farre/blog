#!/bin/bash
# Install gems into a project-local path when the default gem home isn't writable.
if [ ! -w "$(gem env home)" ] && [ "$(bundle config get path 2>/dev/null | grep -c vendor/bundle)" -eq 0 ]; then
  bundle config set --local path 'vendor/bundle'
fi
bundle install
bundle exec jekyll serve