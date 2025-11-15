#!/usr/bin/env nu

let mods_file = ($env.FILE_PWD | path join mods.json)

cat $mods_file
| from json
| each { |mod|
  let url = $'https://api.modrinth.com/v2/project/($mod.slug)/version?loaders=fabric&game_versions=["1.20.1"]&featured=true'
  let json = ^http $url | jq .[0].files.[0] | from json
  { slug: $mod.slug, sha512: $json.hashes.sha512, url: $json.url }
}
| to json
| save -f $mods_file
