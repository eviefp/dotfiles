{ config, lib, dotfiles, pkgs, theme, ... }:
let
  cfg = config.evie.term.tui;
  syntectTheme = pkgs.writeText "theme.tmTheme" /*xml*/ ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
    	<key>name</key>
    	<string>Cyberdream</string>
    	<key>settings</key>
    	<array>
    		<dict>
    			<key>settings</key>
    			<dict>
    				<key>background</key>
    				<string>${theme.dark.fg}</string>
    				<key>caret</key>
    				<string>${theme.dark.bg}</string>
    				<key>block_caret</key>
    				<string>${theme.dark.grey}</string>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    				<key>invisibles</key>
    				<string>${theme.dark.fg}</string>
    				<key>lineHighlight</key>
    				<string>${theme.dark.bg_highlight}</string>
    				<key>selection</key>
    				<string>${theme.dark.bg_highlight}</string>
    				<key>findHighlight</key>
    				<string>${theme.dark.cyan}</string>
    				<key>findHighlightForeground</key>
    				<string>${theme.dark.bg_alt}</string>
    				<key>selectionBorder</key>
    				<string>${theme.dark.fg}</string>
    				<key>activeGuide</key>
    				<string>${theme.dark.orange}</string>
    				<key>bracketsForeground</key>
    				<string>${theme.dark.pink}</string>
    				<key>bracketsOptions</key>
    				<string>underline</string>
    				<key>bracketContentsForeground</key>
    				<string>${theme.dark.bg}</string>
    				<key>bracketContentsOptions</key>
    				<string>underline</string>
    				<key>tagsOptions</key>
    				<string>stippled_underline</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Comment</string>
    			<key>scope</key>
    			<string>comment</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.grey}</string>
    				<key>fontStyle</key>
    				<string></string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>String</string>
    			<key>scope</key>
    			<string>string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.green}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Number</string>
    			<key>scope</key>
    			<string>constant.numeric</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Built-in constant</string>
    			<key>scope</key>
    			<string>constant.language</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>User-defined constant</string>
    			<key>scope</key>
    			<string>constant.character, constant.other</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Variable</string>
    			<key>scope</key>
    			<string>variable</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Ruby's @variable</string>
    			<key>scope</key>
    			<string>variable.other.readwrite.instance</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>String interpolation</string>
    			<key>scope</key>
    			<string>constant.character.escaped, constant.character.escape, string source, string source.ruby</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.magenta}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Ruby Regexp</string>
    			<key>scope</key>
    			<string>source.ruby string.regexp.classic.ruby,source.ruby string.regexp.mod-r.ruby</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Keyword</string>
    			<key>scope</key>
    			<string>keyword</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>
            <dict>
                <key>name</key>
                <string>Keyword Operator</string>
                <key>scope</key>
                <string>keyword.operator</string>
                <key>settings</key>
                <dict>
                    <key>foreground</key>
                    <string>${theme.dark.red}</string>
                </dict>
            </dict>
    		<dict>
    			<key>name</key>
    			<string>Storage</string>
    			<key>scope</key>
    			<string>storage</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Storage type</string>
    			<key>scope</key>
    			<string>storage.type</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Storage Type Namespace</string>
    			<key>scope</key>
    			<string>storage.type.namespace</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Storage Type Class</string>
    			<key>scope</key>
    			<string>storage.type.class</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Class name</string>
    			<key>scope</key>
    			<string>entity.name.class</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>underline</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Meta Path</string>
    			<key>scope</key>
    			<string>meta.path</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>underline</string>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Inherited class</string>
    			<key>scope</key>
    			<string>entity.other.inherited-class</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic underline</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Function name</string>
    			<key>scope</key>
    			<string>entity.name.function</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.blue}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Function argument</string>
    			<key>scope</key>
    			<string>variable.parameter</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Tag name</string>
    			<key>scope</key>
    			<string>entity.name.tag</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Tag attribute</string>
    			<key>scope</key>
    			<string>entity.other.attribute-name</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Library function</string>
    			<key>scope</key>
    			<string>support.function</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.blue}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Library constant</string>
    			<key>scope</key>
    			<string>support.constant</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Library class&#x2f;type</string>
    			<key>scope</key>
    			<string>support.type, support.class</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Library variable</string>
    			<key>scope</key>
    			<string>support.other.variable</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string></string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Support Other Namespace</string>
    			<key>scope</key>
    			<string>support.other.namespace</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Invalid</string>
    			<key>scope</key>
    			<string>invalid</string>
    			<key>settings</key>
    			<dict>
    				<key>background</key>
    				<string>${theme.dark.pink}</string>
    				<key>fontStyle</key>
    				<string></string>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Invalid deprecated</string>
    			<key>scope</key>
    			<string>invalid.deprecated</string>
    			<key>settings</key>
    			<dict>
    				<key>background</key>
    				<string>${theme.dark.red}</string>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON String</string>
    			<key>scope</key>
    			<string>meta.structure.dictionary.json string.quoted.double.json</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>diff.header</string>
    			<key>scope</key>
    			<string>meta.diff, meta.diff.header</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.grey}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>diff.deleted</string>
    			<key>scope</key>
    			<string>markup.deleted</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>diff.inserted</string>
    			<key>scope</key>
    			<string>markup.inserted</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.green}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>diff.changed</string>
    			<key>scope</key>
    			<string>markup.changed</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>scope</key>
    			<string>constant.numeric.line-number.find-in-files - match</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.magenta}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>scope</key>
    			<string>entity.name.filename</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.green}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>scope</key>
    			<string>message.error</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON Punctuation</string>
    			<key>scope</key>
    			<string>punctuation.definition.string.begin.json - meta.structure.dictionary.value.json, punctuation.definition.string.end.json - meta.structure.dictionary.value.json</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON Structure</string>
    			<key>scope</key>
    			<string>meta.structure.dictionary.json string.quoted.double.json</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON String</string>
    			<key>scope</key>
    			<string>meta.structure.dictionary.value.json string.quoted.double.json</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON: 6 deep</string>
    			<key>scope</key>
    			<string>meta meta meta meta meta meta meta.structure.dictionary.value string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON: 5 deep</string>
    			<key>scope</key>
    			<string>meta meta meta meta meta meta.structure.dictionary.value string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.magenta}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON: 4 deep</string>
    			<key>scope</key>
    			<string>meta meta meta meta meta.structure.dictionary.value string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON: 3 deep</string>
    			<key>scope</key>
    			<string>meta meta meta meta.structure.dictionary.value string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.blue}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON: 2 deep</string>
    			<key>scope</key>
    			<string>meta meta meta.structure.dictionary.value string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>JSON:  1 deep</string>
    			<key>scope</key>
    			<string>meta meta.structure.dictionary.value string</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>

    		<!-- Markdown Tweaks -->
    		<dict>
    			<key>name</key>
    			<string>Markup: strike</string>
    			<key>scope</key>
    			<string>markup.strike</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markup: bold</string>
    			<key>scope</key>
    			<string>markup.bold</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>bold</string>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markup: italic</string>
    			<key>scope</key>
    			<string>markup.italic</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: heading</string>
    			<key>scope</key>
    			<string>markup.heading</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.orange}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: List Items Punctuation</string>
    			<key>scope</key>
    			<string>punctuation.definition.list_item.markdown</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: Blockquote</string>
    			<key>scope</key>
    			<string>markup.quote</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: Blockquote Punctuation</string>
    			<key>scope</key>
    			<string>punctuation.definition.blockquote.markdown</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: Separator</string>
    			<key>scope</key>
    			<string>meta.separator</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.grey}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markup: raw inline</string>
    			<key>scope</key>
    			<string>text.html.markdown markup.raw.inline</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.green}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markup: underline</string>
    			<key>scope</key>
    			<string>markup.underline</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>underline</string>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markup: Raw block</string>
    			<key>scope</key>
    			<string>markup.raw.block</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: Raw Block fenced source</string>
    			<key>scope</key>
    			<string>markup.raw.block.fenced.markdown source</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: Fenced Bode Block</string>
    			<key>scope</key>
    			<string>punctuation.definition.fenced.markdown, variable.language.fenced.markdown</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.grey}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Markdown: Fenced Language</string>
    			<key>scope</key>
    			<string>variable.language.fenced.markdown</string>
    			<key>settings</key>
    			<dict>
    				<key>fontStyle</key>
    				<string>italic</string>
    				<key>foreground</key>
    				<string>${theme.dark.grey}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Punctuation Accessor</string>
    			<key>scope</key>
    			<string>punctuation.accessor</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Meta Function Return Type</string>
    			<key>scope</key>
    			<string>meta.function.return-type</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Punctuation Section Block Begin</string>
    			<key>scope</key>
    			<string>punctuation.section.block.begin</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Punctuation Section Block End</string>
    			<key>scope</key>
    			<string>punctuation.section.block.end</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Punctuation Section Embedded Begin</string>
    			<key>scope</key>
    			<string>punctuation.section.embedded.begin</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Punctuation Section Embedded End</string>
    			<key>scope</key>
    			<string>punctuation.section.embedded.end</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Punctuation Separator Namespace</string>
    			<key>scope</key>
    			<string>punctuation.separator.namespace</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Variable Function</string>
    			<key>scope</key>
    			<string>variable.function</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.blue}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Variable Other</string>
    			<key>scope</key>
    			<string>variable.other</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Variable Language</string>
    			<key>scope</key>
    			<string>variable.language</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.red}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Entity Name Module Ruby</string>
    			<key>scope</key>
    			<string>entity.name.module.ruby</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Entity Name Constant Ruby</string>
    			<key>scope</key>
    			<string>entity.name.constant.ruby</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.blue}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Support Function Builtin Ruby</string>
    			<key>scope</key>
    			<string>support.function.builtin.ruby</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.bg}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Storage Type Namespace CS</string>
    			<key>scope</key>
    			<string>storage.type.namespace.cs</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.pink}</string>
    			</dict>
    		</dict>
    		<dict>
    			<key>name</key>
    			<string>Entity Name Namespace CS</string>
    			<key>scope</key>
    			<string>entity.name.namespace.cs</string>
    			<key>settings</key>
    			<dict>
    				<key>foreground</key>
    				<string>${theme.dark.cyan}</string>
    			</dict>
    		</dict>
    	</array>
    	<key>uuid</key>
    	<string>68394a4e-1404-4971-bdfc-81dd7f9d29f6</string>
    	<key>colorSpaceName</key>
    	<string>sRGB</string>
    	<key>semanticClass</key>
    	<string>theme.cyberdream</string>
    	<key>author</key>
    	<string>Scott McKendry</string>
    </dict>
    </plist>
  '';

in
{
  options.evie.term.tui = {
    enable = lib.mkEnableOption "term tui";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      dotfiles.gitu.packages.${pkgs.system}.default
      dotfiles.porc.packages.${pkgs.system}.default
      pkgs.calc
    ];

    programs = {
      bottom = {
        enable = true;
        settings = {
          left_legend = true;
          temperature_type = "c";
          color = "gruvbox";
        };
      };

      yazi = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        settings = {
          mgr = {
            sort_by = "natural";
            show_hidden = true;
          };
          open = {
            prepend_rules = [
              {
                name = "*.pdf";
                use = "zathura";
              }
            ];
          };
          opener = {
            zathura = [
              {
                run = "zathura";
                orphan = false;
                for = "unix";
              }
            ];
          };
        };

        theme = {
          manager = {
            synect_theme = "${syntectTheme}";
            border_style = { fg = "${theme.dark.bg_highlight}"; };
            cwd = { fg = "${theme.dark.cyan}"; };
            find_keyword = { bold = true; fg = "${theme.dark.green}"; };
            find_position = { fg = "${theme.dark.fg}"; };
            hovered = { bg = "${theme.dark.grey}"; bold = true; fg = "${theme.dark.fg}"; };
            marker_copied = { bg = "${theme.dark.yellow}"; fg = "${theme.dark.yellow}"; };
            marker_cut = { bg = "${theme.dark.red}"; fg = "${theme.dark.red}"; };
            marker_selected = { bg = "${theme.dark.bg_highlight}"; fg = "${theme.dark.green}"; };
            preview_hovered = { bg = "${theme.dark.bg_highlight}"; bold = true; fg = "${theme.dark.fg}"; };
            tab_active = { bg = "${theme.dark.blue}"; fg = "${theme.dark.fg}"; };
            tab_inactive = { bg = "${theme.dark.bg_highlight}"; fg = "${theme.dark.bg}"; };

            count_selected = { bg = "${theme.dark.green}"; fg = "${theme.dark.fg}"; };
            count_copied = { bg = "${theme.dark.yellow}"; fg = "${theme.dark.fg}"; };
            count_cut = { bg = "${theme.dark.red}"; fg = "${theme.dark.fg}"; };
          };

          completion = {
            active = { bg = "${theme.dark.grey}"; fg = "${theme.dark.red}"; };
            border = { fg = "${theme.dark.blue}"; };
            inactive = { fg = "${theme.dark.bg}"; };
          };

          filetype = {
            rules = [
              { fg = "${theme.dark.cyan}"; mime = "image/*"; }
              { fg = "${theme.dark.yellow}"; mime = "video/*"; }
              { fg = "${theme.dark.yellow}"; mime = "audio/*"; }
              { fg = "${theme.dark.red}"; mime = "application/zip"; }
              { fg = "${theme.dark.red}"; mime = "application/gzip"; }
              { fg = "${theme.dark.red}"; mime = "application/x-tar"; }
              { fg = "${theme.dark.red}"; mime = "application/x-bzip"; }
              { fg = "${theme.dark.red}"; mime = "application/x-bzip2"; }
              { fg = "${theme.dark.red}"; mime = "application/x-7z-compressed"; }
              { fg = "${theme.dark.red}"; mime = "application/x-rar"; }
              { fg = "${theme.dark.red}"; mime = "application/xz"; }
              { fg = "${theme.dark.green}"; mime = "application/doc"; }
              { fg = "${theme.dark.green}"; mime = "application/pdf"; }
              { fg = "${theme.dark.green}"; mime = "application/rtf"; }
              { fg = "${theme.dark.green}"; mime = "application/vnd.*"; }
              { bold = true; fg = "${theme.dark.blue}"; mime = "inode/directory"; }
              { fg = "${theme.dark.fg}"; mime = "*"; }
            ];
          };

          help = {
            desc = { fg = "${theme.dark.fg}"; };
            footer = { fg = "${theme.dark.fg}"; };
            hovered = { bg = "${theme.dark.grey}"; fg = "${theme.dark.fg}"; };
            on = { fg = "${theme.dark.red}"; };
            run = { fg = "${theme.dark.cyan}"; };
          };

          input = {
            border = { fg = "${theme.dark.blue}"; };
            selected = { bg = "${theme.dark.grey}"; };
            title = { fg = "${theme.dark.fg}"; };
            value = { fg = "${theme.dark.fg}"; };
          };

          select = {
            active = { fg = "${theme.dark.red}"; };
            border = { fg = "${theme.dark.blue}"; };
            inactive = { fg = "${theme.dark.fg}"; };
          };

          status = {
            mode_normal = { bg = "${theme.dark.blue}"; bold = true; fg = "${theme.dark.fg}"; };
            mode_select = { bg = "${theme.dark.green}"; bold = true; fg = "${theme.dark.fg}"; };
            mode_unset = { bg = "${theme.dark.magenta}"; bold = true; fg = "${theme.dark.fg}"; };
            permissions_r = { fg = "${theme.dark.yellow}"; };
            permissions_s = { fg = "${theme.dark.cyan}"; };
            permissions_t = { fg = "${theme.dark.blue}"; };
            permissions_w = { fg = "${theme.dark.red}"; };
            permissions_x = { fg = "${theme.dark.green}"; };
            progress_error = { bg = "${theme.dark.fg}"; fg = "${theme.dark.red}"; };
            progress_label = { bg = "${theme.dark.fg}"; fg = "${theme.dark.bg}"; };
            progress_normal = { bg = "${theme.dark.fg}"; fg = "${theme.dark.bg}"; };
            separator_style = { bg = "${theme.dark.bg_highlight}"; fg = "${theme.dark.bg_highlight}"; };
          };

          stasks = {
            border = { fg = "${theme.dark.blue}"; };
            hovered = { bg = "${theme.dark.grey}"; fg = "${theme.dark.bg}"; };
            title = { fg = "${theme.dark.bg}"; };
          };

          which = {
            cand = { fg = "${theme.dark.cyan}"; };
            desc = { fg = "${theme.dark.bg}"; };
            mask = { bg = "${theme.dark.bg_highlight}"; };
            rest = { fg = "${theme.dark.magenta}"; };
            separator_style = { fg = "${theme.dark.grey}"; };
          };
        };

        # keymap = {
        #   manager.keymap = [
        #     { run = "escape"; on = "<Esc>"; }
        #     { run = "quit"; on = "q"; }
        #
        #     { run = "arrow -1"; on = "k"; }
        #     { run = "arrow 1"; on = "j"; }
        #
        #     { run = "arrow -50%"; on = "<C-u>"; }
        #     { run = "arrow 50%"; on = "<C-d>"; }
        #
        #     { run = "arrow -99999999"; on = [ "g" "g" ]; }
        #     { run = "arrow 99999999"; on = "G"; }
        #
        #     { run = "leave"; on = "h"; }
        #     { run = "enter"; on = "l"; }
        #
        #     { run = [ "toggle" "arrow 1" ]; on = "<Space>"; }
        #     { run = "visual_mode"; on = "v"; }
        #     { run = "visual_mode --unset"; on = "V"; }
        #
        #     { run = "seek -5"; on = "K"; }
        #     { run = "seek 5"; on = "J"; }
        #
        #     { run = "open"; on = "o"; }
        #     { run = "open --interactive"; on = "O"; }
        #   ];
        # };
      };

    };

    home.file.".config/gitu/config.toml".text = /*toml*/ ''

# This file contains Gitu's default configuration.
# It is possible to override settings with an equivalent file at:
# `~/.config/gitu/config.toml`

[general]
always_show_help.enabled = false
confirm_quit.enabled = false

[style]
# fg / bg can be either of:
# - a hex value: "#707070"
# - an ansi color name: "light blue"
# - an ansi color index: "255"
# - "reset" will set the terminal's default foreground / background color.

# 'mods' can be any combination of (multiple values separated by '|'):
# "BOLD|DIM|ITALIC|UNDERLINED|SLOW_BLINK|RAPID_BLINK|REVERSED|HIDDEN|CROSSED_OUT"

# Example style config values:
# section_header = { fg = "#808080" }
# section_header = { bg = "light green", mods = "UNDERLINED|ITALIC" }

section_header = { fg = "${theme.dark.yellow}" }
file_header = { fg = "${theme.dark.magenta}" }
hunk_header = { fg = "${theme.dark.blue}" }

diff_highlight.tag_old = { fg = "${theme.dark.red}", mods = "BOLD" }
diff_highlight.tag_new = { fg = "${theme.dark.green}", mods = "BOLD" }
diff_highlight.unchanged_old = { mods = "DIM" }
diff_highlight.unchanged_new = { mods = "DIM" }
diff_highlight.changed_old = { fg = "${theme.dark.red}" }
diff_highlight.changed_new = { fg = "${theme.dark.green}"}

syntax_highlight.enabled = true
syntax_highlight.attribute = { fg = "${theme.dark.yellow}" }
syntax_highlight.comment = { fg = "${theme.dark.grey}" }
syntax_highlight.constant_builtin = {}
syntax_highlight.constant = {}
syntax_highlight.constructor = {}
syntax_highlight.embedded = {}
syntax_highlight.function_builtin = { fg = "${theme.dark.cyan}" }
syntax_highlight.function = { fg = "${theme.dark.blue}" }
syntax_highlight.keyword = { fg = "${theme.dark.magenta}" }
syntax_highlight.number = {}
syntax_highlight.module = { fg = "${theme.dark.cyan}" }
syntax_highlight.property = {}
syntax_highlight.operator = {}
syntax_highlight.punctuation_bracket = {}
syntax_highlight.punctuation_delimiter = {}
syntax_highlight.string_special = { fg = "${theme.dark.yellow}" }
syntax_highlight.string = { fg = "${theme.dark.yellow}" }
syntax_highlight.tag = {}
syntax_highlight.type = { fg = "${theme.dark.yellow}" }
syntax_highlight.type_builtin = { fg = "${theme.dark.yellow}" }
syntax_highlight.variable_builtin = {}
syntax_highlight.variable_parameter = {}

cursor = { fg = "${theme.dark.blue}" }
selection_bar = { fg = "${theme.dark.blue}", mods = "DIM" }
selection_line = { mods = "BOLD" }
# You may want to set `selection_area.bg` to a nice background color.
# Looks horrible with regular terminal colors, so is therefore not set.
selection_area = {}

hash = { fg = "${theme.dark.yellow}" }
branch = { fg = "${theme.dark.green}" }
remote = { fg = "${theme.dark.red}" }
tag = { fg = "${theme.dark.yellow}" }

command = { fg = "${theme.dark.blue}", mods = "BOLD" }
active_arg = { fg = "${theme.dark.red}", mods = "BOLD" }
hotkey = { fg = "${theme.dark.magenta}" }

[bindings]
root.quit = ["q", "<esc>"]
root.refresh = ["g"]
root.toggle_section = ["<tab>"]
root.move_up = ["k", "<up>"]
root.move_down = ["j", "<down>"]
root.move_up_line = ["<ctrl+k>", "<ctrl+up>"]
root.move_down_line = ["<ctrl+j>", "<ctrl+down>"]
root.move_prev_section = ["<alt+k>", "<alt+up>"]
root.move_next_section = ["<alt+j>", "<alt+down>"]
root.move_parent_section = ["<alt+h>", "<alt+left>"]
root.half_page_up = ["<ctrl+u>"]
root.half_page_down = ["<ctrl+d>"]
root.show_refs = ["Y"]
root.show = ["<enter>"]
root.discard = ["K"]
root.stage = ["s"]
root.unstage = ["u"]
root.copy_hash = ["y"]

root.help_menu = ["h"]
help_menu.quit = ["q", "<esc>"]

root.branch_menu = ["b"]
branch_menu.checkout = ["b"]
branch_menu.checkout_new_branch = ["c"]
branch_menu.quit = ["q", "<esc>"]

root.commit_menu = ["c"]
commit_menu.--all = ["-a"]
commit_menu.--allow-empty = ["-e"]
commit_menu.--verbose = ["-v"]
commit_menu.--no-verify = ["-n"]
commit_menu.--reset-author = ["-R"]
commit_menu.--signoff = ["-s"]
commit_menu.commit = ["c"]
commit_menu.commit_amend = ["a"]
commit_menu.commit_fixup = ["f"]
commit_menu.quit = ["q", "<esc>"]

root.fetch_menu = ["f"]
fetch_menu.--prune = ["-p"]
fetch_menu.--tags = ["-t"]
fetch_menu.fetch_all = ["a"]
fetch_menu.quit = ["q", "<esc>"]
fetch_menu.fetch_elsewhere = ["e"]

root.log_menu = ["l"]
log_menu.log_current = ["l"]
log_menu.log_other = ["o"]
log_menu.quit = ["q", "<esc>"]
log_menu.-n = ["-n"]
log_menu.--grep = ["-F"]

root.pull_menu = ["F"]
pull_menu.--rebase = ["-r"]
pull_menu.pull = ["p"]
pull_menu.pull_elsewhere = ["e"]
pull_menu.quit = ["q", "<esc>"]

root.push_menu = ["P"]
push_menu.--force-with-lease = ["-f"]
push_menu.--force = ["-F"]
push_menu.--no-verify = ["-h"]
push_menu.--dry-run = ["-n"]
push_menu.push = ["p"]
push_menu.push_elsewhere = ["e"]
push_menu.quit = ["q", "<esc>"]

root.rebase_menu = ["r"]
rebase_menu.--keep-empty = ["-k"]
rebase_menu.--preserve-merges = ["-p"]
rebase_menu.--committer-date-is-author-date = ["-d"]
rebase_menu.--autosquash = ["-a"]
rebase_menu.--autostash = ["-A"]
rebase_menu.--interactive = ["-i"]
rebase_menu.--no-verify = ["-h"]
rebase_menu.rebase_interactive = ["i"]
rebase_menu.rebase_abort = ["a"]
rebase_menu.rebase_continue = ["c"]
rebase_menu.rebase_elsewhere = ["e"]
rebase_menu.rebase_autosquash = ["f"]
rebase_menu.quit = ["q", "<esc>"]

root.reset_menu = ["X"]
reset_menu.reset_soft = ["s"]
reset_menu.reset_mixed = ["m"]
reset_menu.reset_hard = ["h"]
reset_menu.quit = ["q", "<esc>"]

root.revert_menu = ["V"]
revert_menu.--edit = ["-e"]
revert_menu.--no-edit = ["-E"]
revert_menu.--signoff = ["-s"]
revert_menu.revert_abort = ["a"]
revert_menu.revert_continue = ["c"]
revert_menu.revert_commit = ["V"]
revert_menu.quit = ["q", "<esc>"]

root.stash_menu = ["z"]
stash_menu.--all = ["-a"]
stash_menu.--include-untracked = ["-u"]
stash_menu.stash = ["z"]
stash_menu.stash_index = ["i"]
stash_menu.stash_worktree = ["w"]
stash_menu.stash_keep_index = ["x"]
stash_menu.stash_pop = ["p"]
stash_menu.stash_apply = ["a"]
stash_menu.stash_drop = ["k"]
stash_menu.quit = ["q", "<esc>"]
    '';

  };
}
