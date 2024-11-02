#!/usr/bin/env nu

def main [type: string] {
  let format = '{{ "title": "{title}", "date": "{start-date}", "time": "{start-time}", "end": "{end-date}", "endTime": "{end-time}", "status": "{status}", "location": "{location}", "duration": "TODO", "source": "{calendar}" }}'
  let nextWeek = 'next week' | into datetime | format date "%d/%m/%Y"

  let jqArgs = match $type {
    'single' => '.[1]',
    _        => '.'
  }

  khal list --format ($format) --notstarted --day-format "" today ($nextWeek) | jq -scM ($jqArgs)
}
