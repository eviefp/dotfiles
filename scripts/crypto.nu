export def encrypt [ file ] {
  gpg --encrypt --armor--recipient $env.KEYID_EVIE $file
}

export def decrypt [ file ] {
  gpg --decrypt --armor $file
}

export def sign [] {
  gpg --armor --clearsign
}

export def verify [ file ] {
  gpg --verify $file
}
