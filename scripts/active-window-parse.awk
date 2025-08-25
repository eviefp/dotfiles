#!/usr/bin/env -S awk -f

function mr(text, what, extra) {
  if(match(text, what)) {
    sub(what,  "", text)
    return extra text
  }
  return ""
}

function firefox(title) {
  sub(/ — Mozilla Firefox$/, "", title)

  t = mr(title, " - Twitch$", " ")
  if (t != "") { return t }

  t = mr(title, " - YouTube$", " ")
  if (t != "") { return t }

  t = mr(title, " - YouTube$", " ")
  if (t != "") { return t }

  t = mr(title, "• Discord \\| ", "  ")
  if (t != "") { return t }

  t = mr(title, "— Bluesky$", " ")
  if (t != "") { return t }

  t = mr(title, "^WhatsApp$", " whatsapp")
  if (t != "") { return t }

  return "🌐 " title
}

function kitty(title) {
  t = mr(title, " vi$", " ")
  if (t != "") { return t }

  t = mr(title, "^~/code/", " ")
  if (t != "") { return t }

  return "🐈 " title
}

function main(class, title) {
  if(tolower(class) == "firefox") {
    return firefox(title)
  } else if(tolower(class) == "kitty") {
    return kitty(title)
  } else return tolower(class)
}

/^activewindow>>/{ print main($2,$3) }
