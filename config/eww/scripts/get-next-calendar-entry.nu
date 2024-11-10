#!/usr/bin/env nu

def main [type: string] {
  let format = '{{ "title": "{title}", "date": "{start-date}", "time": "{start-time}", "end": "{end-date}", "endTime": "{end-time}", "status": "{status}", "location": "{location}", "source": "{calendar}" }}'
  let nextWeek = 'next week' | into datetime | format date "%d/%m/%Y"
  let calendars = [['name' 'calendar']; ['Evie' 'alexaeviest@gmail.com'] ['Evie & Every' 's810p67l2bi1168j8luka5nic0@group.calendar.google.com'] ['garnix' 'eciobanu@garnix.io'] ['holidays' 'cln2ssjfdlgmsqb1dohmgrrcd5i62ua0ctp6utbg5pr2sor1dhimsp31e8n6errfctm6abj3dtmg@virtual']]
  let onlyCalendars = $calendars | reduce --fold [] {|it, acc| [ ...$acc '-a' $it.calendar ]}

  let jqQuery = '
def diff($finish; $start):
  def twodigits: "00" + tostring | .[-2:];
  [$finish, $start]
  | map(strptime("%d/%m/%Y %H:%M") | mktime) # seconds
  | .[0] - .[1]
  | (. % 60 | twodigits) as $s
  | (((. / 60) % 60) | twodigits)  as $m
  | (./3600 | floor) as $h
  | "\($h)h\($m)m" ;
                '
  let jqArgs = match $type {
    'single' => ' .[0]',
    _        => '.'
  }

  let extraArgs = match $type {
    'single' => [ '--once' '--notstarted'],
    _        => [ ],
  }

  let jqArgsWithDuration = $jqQuery + '
      .[]
    | (select (.time == "")).time |= "00:00"
    | (select (.endTime == "")).endTime |= "00:00"
    | (select (.end == "")).end |= .date
    | . += {startDT: (.date + " " + .time)}
    | . += {endDT: (.end + " " + .endTime)}
    | . += {duration: diff(.endDT;.startDT)}'


  let json = (khal list ...$onlyCalendars --format ($format) ...$extraArgs --day-format "" now ($nextWeek)
    | jq -scM ($jqArgsWithDuration)
    | jq -scM ($jqArgs)
  )

  if $type != 'single' {
    let result = $json | from json
    let now = 'now' | into datetime

    $result | each {|it|
          if $it.date == ($now | format date "%d/%m/%Y") {
            if $it.time == ($now + 15min | format date "%H:%M") {
              notify-send $it.title
            }
          }
      }
  }

  ($calendars
    | reduce --fold $json {|it, acc| $acc | sd -F $it.calendar $it.name}
  )
}
