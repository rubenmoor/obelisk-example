<?xml version="1.0" encoding="UTF-8"?>
<rss xmlns:atom="http://www.w3.org/2005/Atom"
     xmlns:content="http://purl.org/rss/1.0/modules/content/"
     xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd"
     xmlns:media="http://search.yahoo.com/mrss/"
     xmlns:creativeCommons="http://backend.userland.com/creativeCommonsRssModule"
     version="2.0">

    <channel>
        <title>#{podcastTitle}</title>
        <link>#{podcastUrl}</link>
        <language>en-us</language>
        <atom:link href="#{url}/feed.xml"
                   rel="self"
                   type="application/rss+xml"/>
        <copyright>#{podcastCopyright}</copyright>
        <description>#{podcastDescription}</description>
        <managingEditor>#{podcastEmail}</managingEditor>
        <webMaster>#{podcastEmail}</webMaster>
        <creativeCommons:license>#{podcastLicence}</creativeCommons:license>
        <pubDate>#{pubDate}</pubDate>
        <lastBuildDate>#{latestDate}</lastBuildDate>
        <image>
            <url>#{imgUrl}</url>
            <title>#{podcastTitle}</title>
            <link>#{podcastUrl}</link>
        </image>
        <docs>http://www.rssboard.org/rss-specification</docs>
        <itunes:subtitle>#{podcastItunesSubtitle}</itunes:subtitle>
        <itunes:author>#{podcastAuthors}</itunes:author>
        <itunes:summary>#{podcastItunesSummary}</itunes:summary>
        <itunes:keywords>${podcastKeywords}</itunes:keywords>
        <itunes:block>yes</itunes:block>
        <itunes:owner>
            <itunes:name><![CDATA[#{podcastItunesOwnerNames}]]></itunes:name>
            <itunes:email>#{podcastEmail}</itunes:email>
        </itunes:owner>
        <itunes:image href="#{imgUrl}" />
        <itunes:category text="Technology" />
        <itunes:explicit>no</itunes:explicit>
        %{ forall efd <- episodeData }
        <item>
            <title>#{efdTitle efd}</title>
            <link>#{efdPageUrl efd}</link>
            <description>#{efdDescription efd}</description>
            <guid isPermaLink="false">#{efdPageUrl efd}</guid>
            <pubDate>#{efdRFC822 efd}</pubDate>
            <media:content
                medium="audio"
                url="#{efdAudioFileUrl efd}"
                type="#{efdAudioContentType efd}"
                isDefault="true"
                duration="#{efdDurationSeconds efd}">
            </media:content>
            <media:title type="plain">#{efdTitle efd}</media:title>
            <media:description>#{efdDescription efd}</media:description>
            <media:rating scheme="urn:simple">adult</media:rating>
            <media:keywords>moral</media:keywords>
            <enclosure url="#{efdAudioFileUrl efd}" length="#{efdFileSize efd}" type="#{efdAudioContentType efd}" />
            <media:thumbnail url="#{efdThumbnailFile efd}"/>
            <itunes:image href="#{efdThumbnailFile efd}" />
            <itunes:duration>#{efdDurationFormatted efd}</itunes:duration>
            <itunes:explicit>yes</itunes:explicit>
        </item>
        %{ endforall }
    </channel>
</rss>
