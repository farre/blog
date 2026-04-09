module Jekyll
  class TagPage < Page
    def initialize(site, base, tag)
      @site = site
      @base = base
      @dir = File.join('tags', tag)
      @name = 'index.html'

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'tag.html')
      self.data['tag'] = tag
      self.data['title'] = "Posts tagged \"#{tag}\""
    end
  end

  class TagFeed < Page
    def initialize(site, base, tag)
      @site = site
      @base = base
      @dir = 'rss'
      @name = "#{tag}.xml"

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'tag-feed.xml')
      self.data['tag'] = tag
      self.data['title'] = "#{tag} — #{site.config['title']}"
    end
  end

  class TagFeedSummary < Page
    def initialize(site, base, tag)
      @site = site
      @base = base
      @dir = 'rss'
      @name = "#{tag}-summary.xml"

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'tag-feed-summary.xml')
      self.data['tag'] = tag
      self.data['title'] = "#{tag} (summary) — #{site.config['title']}"
    end
  end

  class TagPageGenerator < Generator
    safe true

    def generate(site)
      if site.layouts.key? 'tag'
        site.tags.each_key do |tag|
          site.pages << TagPage.new(site, site.source, tag)
          site.pages << TagFeed.new(site, site.source, tag)
          site.pages << TagFeedSummary.new(site, site.source, tag)
        end
      end
    end
  end
end
